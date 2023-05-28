module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (MixerConfig, MixerDatum (..), MixerRedeemer (..), PAssetClass, PCommitment)
import Plutarch.Api.V1.Value (plovelaceValueOf, pvalueOf)
import Plutarch.Api.V2 (
  PDatum,
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOut,
 )
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
import Plutarch.Extra.TermCont (pguardC, pletFieldsC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

validatorLogic ::
  forall s.
  Term
    s
    ( PAsData MixerConfig
        :--> PAsData MixerDatum
        :--> PAsData MixerRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
validatorLogic = plam \(pfromData -> config) (pfromData -> d) (pfromData -> r) ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "outputs", "mint", "datums", "signatories"] $ ctx.txInfo
  -- Find own input:
  PSpending i <- pmatch ctx.purpose
  let ownInputRef = pfield @"_0" # i
  PJust ownInput <- pmatch $ pfindOwnInput # info.inputs # ownInputRef
  ownInputResolved <- plet (pfield @"resolved" # ownInput)
  -- Find own output:
  ownOutput <- pletFields @'["value", "datum"] $ pgetOnlyOneOutputFromList #$ pgetContinuingOutputs # info.outputs # ownInputResolved
  -- Get produced datum:
  (nextState, _) <- ptryFrom @MixerDatum $ inlineDatumFromOutput # ownOutput.datum
  outputState <- pletFields @'["nullifierHashes"] nextState
  outputValue <- plet (ownOutput.value)
  -- Check protocol token:
  conf <- pletFields @'["protocolToken", "poolNominal"] config
  PUnit <- pmatch $ containsOneProtocolToken # conf.protocolToken # outputValue
  let inputValue = pfield @"value" # ownInputResolved
  inputState <- pletFields @'["nullifierHashes"] d
  -- Allowed transitions given a redeemer:
  pmatch r \case
    Deposit c -> unTermCont do
      let commit = pfield @"commitment" # c
      validateDeposit conf inputState outputState inputValue outputValue commit
    _ -> ptraceError "Not implemented"

validateDeposit ::
  ( PMemberFields MixerDatum '["nullifierHashes"] s datum
  , PMemberFields MixerConfig '["poolNominal"] s config
  ) =>
  HRec config ->
  HRec datum ->
  HRec datum ->
  Term s SortedPositiveValue ->
  Term s SortedPositiveValue ->
  Term s PCommitment ->
  TermCont s (Term s PUnit)
validateDeposit conf inputState outputState inputValue outputValue commit = do
  pguardC "Nullifier hash list modified" (inputState.nullifierHashes #== outputState.nullifierHashes)
  let depositedAmount = (plovelaceValueOf # outputValue) - (plovelaceValueOf # inputValue)
  pguardC "Nominal amount should be paid to script" (depositedAmount #== conf.poolNominal)
  pure $ pconstant ()

containsOneProtocolToken ::
  Term
    s
    ( PAssetClass
        :--> SortedPositiveValue
        :--> PUnit
    )
containsOneProtocolToken = phoistAcyclic $
  plam $ \assetClass val -> unTermCont do
    protocolToken <- pletFieldsC @'["_0", "_1"] assetClass
    pguardC "Output should contain one protocol token" $ (pvalueOf # val # protocolToken._0 # protocolToken._1) #== 1
    pure $ pconstant ()
