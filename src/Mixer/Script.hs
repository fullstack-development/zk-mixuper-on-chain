module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (filterInputsByToken, filterOutputsByToken, inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (PMixerDatum (PDepositTree, PVault), PWithdrawConfig, PWithdrawDatum (..), PWithdrawRedeemer (..))
import Mixer.Script.Withdraw (validateWithdraw)
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, plovelaceValueOf, pvalueOf)
import Plutarch.Api.V2 (
  PDatum,
  PMaybeData (..),
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.Builtin (pasInt, pforgetData)
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
import Plutarch.Extra.Maybe (pfromDJust)
import Plutarch.Extra.TermCont (pguardC, pletFieldsC, pmatchC, ptryFromC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore.Data (Data)

mkValidator ::
  Data ->
  ClosedTerm PValidator
mkValidator config =
  validatorLogic # P.do
    pdat <- plet $ pconstant @PData config
    (pconfig, _) <- ptryFrom @(PAsData PWithdrawConfig) pdat
    pconfig

validatorLogic ::
  forall s.
  Term
    s
    ( PAsData PWithdrawConfig
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
validatorLogic = plam \(pfromData -> config) d r ctx' -> P.do
  (oldState, _) <- ptryFrom @PWithdrawDatum d
  (redeemer, _) <- ptryFrom @PWithdrawRedeemer r
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "outputs", "referenceInputs", "signatories"] $ ctx.txInfo
  -- Find own input:
  PSpending i <- pmatch ctx.purpose
  let ownInputRef = pfield @"_0" # i
  PJust ownInput <- pmatch $ pfindOwnInput # info.inputs # ownInputRef
  ownInputResolved <- plet (pfield @"resolved" # ownInput)
  -- Find own output:
  ownOutput <- pletFields @'["value", "datum"] $ pgetOnlyOneOutputFromList #$ pgetContinuingOutputs # info.outputs # ownInputResolved
  -- Get produced datum:
  (nextState, _) <- ptryFrom @PWithdrawDatum $ inlineDatumFromOutput # ownOutput.datum
  outputState <- pletFields @'["nullifierHashes"] nextState
  -- Check protocol token:
  conf <- pletFields @'["protocolCurrency", "depositTreeTokenName", "vaultTokenName", "nullifierStoreTokenName", "poolNominal", "vk"] config
  PUnit <- pmatch $ containsOneProtocolToken # conf.protocolCurrency # conf.nullifierStoreTokenName # ownOutput.value
  inputState <- pletFields @'["nullifierHashes"] oldState
  -- Get deposit tree reference input:
  let depositTreeInput = pgetOnlyOneOutputFromList #$ filterInputsByToken # conf.protocolCurrency # conf.depositTreeTokenName # info.referenceInputs
  (mixerDatum, _) <- ptryFrom @PMixerDatum (inlineDatumFromOutput #$ pfield @"datum" # depositTreeInput)
  PDepositTree depositTree <- pmatch mixerDatum
  let treeRoot = pfield @"merkleTreeRoot" #$ pfield @"depositTree" # depositTree
  -- Check vault input and output:
  inputValue <-
    plet $
      pfield @"value"
        #$ pgetOnlyOneOutputFromList
        #$ filterInputsByToken
        # conf.protocolCurrency
        # conf.vaultTokenName
        # info.inputs
  -- TODO! vault output should have the same address as vault input, otherwise script should fail
  vaultOutput <-
    pletFields @'["value", "datum"] $
      pgetOnlyOneOutputFromList #$ filterOutputsByToken # conf.protocolCurrency # conf.vaultTokenName # info.outputs
  (vaultDatum, _) <- ptryFrom @PMixerDatum (inlineDatumFromOutput # vaultOutput.datum)
  PVault _ <- pmatch vaultDatum
  -- Validate withdraw:
  rdm <- pletFields @'["publicInput", "proof"] redeemer
  popaque $
    unTermCont $
      validateWithdraw conf ctx.txInfo inputState outputState inputValue vaultOutput.value treeRoot rdm

containsOneProtocolToken ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> SortedPositiveValue
        :--> PUnit
    )
containsOneProtocolToken = phoistAcyclic $
  plam $ \cur tn val -> unTermCont do
    pguardC "Output should contain one protocol token" $ (pvalueOf # val # cur # tn) #== 1
    pure $ pconstant ()
