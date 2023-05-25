module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (MixerConfig, MixerDatum (..), MixerRedeemer (..), PCommitment)
import Plutarch.Api.V2 (
  PDatum,
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOut,
 )
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
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
validatorLogic = plam $ \(pfromData -> conf) (pfromData -> d) (pfromData -> r) ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "outputs", "mint", "datums", "signatories"] $ ctx.txInfo
  -- Find own input:
  PSpending i <- pmatch ctx.purpose
  let ownInputRef = pfield @"_0" # i
  PJust ownInput <- pmatch $ pfindOwnInput # info.inputs # ownInputRef
  plet (pfield @"resolved" # ownInput) \ownInputResolved -> P.do
    -- Find own output:
    ownOutput <- pletFields @'["value", "datum"] $ pgetOnlyOneOutputFromList #$ pgetContinuingOutputs # info.outputs # ownInputResolved
    -- Get produced datum:
    (nextState, _) <- ptryFrom @MixerDatum $ inlineDatumFromOutput # ownOutput.datum
    outputState <- pletFields @'["nullifierHashes"] nextState
    -- Allowed transitions given a redeemer:
    plet (ownOutput.value) $ \outputValue -> P.do
      let inputValue = pfield @"value" # ownInputResolved
      inputState <- pletFields @'["nullifierHashes"] d
      pmatch r \case
        Deposit c -> unTermCont do
          let commit = pfield @"commitment" # c
          validateDeposit inputState outputState inputValue outputValue commit
        _ -> ptraceError "Not implemented"

validateDeposit ::
  PMemberFields MixerDatum '["nullifierHashes"] s datum =>
  HRec datum ->
  HRec datum ->
  Term s SortedPositiveValue ->
  Term s SortedPositiveValue ->
  Term s PCommitment ->
  TermCont s (Term s PUnit)
validateDeposit inputState outputState inputValue outputValue commit = do
  -- Do checks with info fields here.
  pure $ pconstant ()
