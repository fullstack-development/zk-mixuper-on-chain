module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (filterInputsByToken, filterOutputsByToken, inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (PMixerDatum (PDepositTree, PVault), PWithdrawConfig, PWithdrawDatum (..), PWithdrawRedeemer (..))
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, plovelaceValueOf, pvalueOf)
import Plutarch.Api.V2 (
  PDatum,
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
import Plutarch.Extra.TermCont (pguardC, pletFieldsC)
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
  info <- pletFields @'["inputs", "outputs", "referenceInputs"] $ ctx.txInfo
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
  conf <- pletFields @'["protocolCurrency", "depositTreeTokenName", "vaultTokenName", "nullifierStoreTokenName", "poolNominal"] config
  PUnit <- pmatch $ containsOneProtocolToken # conf.protocolCurrency # conf.nullifierStoreTokenName # ownOutput.value
  inputState <- pletFields @'["nullifierHashes"] oldState
  -- Get deposit tree reference input:
  let depositTreeInput = pgetOnlyOneOutputFromList #$ filterInputsByToken # conf.protocolCurrency # conf.depositTreeTokenName # info.referenceInputs
  (mixerDatum, _) <- ptryFrom @PMixerDatum (inlineDatumFromOutput #$ pfield @"datum" # depositTreeInput)
  PDepositTree depositTree <- pmatch mixerDatum
  -- Check vault input and output:
  vaultInput <-
    pletFields @'["value", "datum"] $
      pgetOnlyOneOutputFromList #$ filterInputsByToken # conf.protocolCurrency # conf.vaultTokenName # info.inputs
  vaultOutput <-
    pletFields @'["value", "datum"] $
      pgetOnlyOneOutputFromList #$ filterOutputsByToken # conf.protocolCurrency # conf.vaultTokenName # info.outputs
  (vaultDatum, _) <- ptryFrom @PMixerDatum (inlineDatumFromOutput # vaultOutput.datum)
  PVault _ <- pmatch vaultDatum
  -- Validate withdraw:
  popaque $
    unTermCont $
      validateWithdraw conf inputState outputState vaultInput.value vaultOutput.value

validateWithdraw ::
  ( PMemberFields PWithdrawDatum '["nullifierHashes"] s datum
  , PMemberFields PWithdrawConfig '["poolNominal"] s config
  ) =>
  HRec config ->
  HRec datum ->
  HRec datum ->
  Term s SortedPositiveValue ->
  Term s SortedPositiveValue ->
  TermCont s (Term s PUnit)
validateWithdraw conf inputState outputState inputValue outputValue = do
  pure $ pconstant ()

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
