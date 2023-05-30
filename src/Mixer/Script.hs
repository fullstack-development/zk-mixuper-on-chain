module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (PAssetClass, PCommitment, PMixerConfig, PMixerDatum (..), PMixerRedeemer (..))
import Plutarch.Api.V1.Value (plovelaceValueOf, pvalueOf)
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
    (pconfig, _) <- ptryFrom @(PAsData PMixerConfig) pdat
    pconfig

validatorLogic ::
  forall s.
  Term
    s
    ( PAsData PMixerConfig
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
validatorLogic = plam \(pfromData -> config) d r ctx' -> P.do
  (oldState, _) <- ptryFrom @PMixerDatum d
  (redeemer, _) <- ptryFrom @PMixerRedeemer r
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "outputs"] $ ctx.txInfo
  -- Find own input:
  PSpending i <- pmatch ctx.purpose
  let ownInputRef = pfield @"_0" # i
  PJust ownInput <- pmatch $ pfindOwnInput # info.inputs # ownInputRef
  ownInputResolved <- plet (pfield @"resolved" # ownInput)
  -- Find own output:
  ownOutput <- pletFields @'["value", "datum"] $ pgetOnlyOneOutputFromList #$ pgetContinuingOutputs # info.outputs # ownInputResolved
  -- Get produced datum:
  (nextState, _) <- ptryFrom @PMixerDatum $ inlineDatumFromOutput # ownOutput.datum
  outputState <- pletFields @'["nullifierHashes"] nextState
  outputValue <- plet (ownOutput.value)
  -- Check protocol token:
  conf <- pletFields @'["protocolToken", "poolNominal"] config
  PUnit <- pmatch $ containsOneProtocolToken # conf.protocolToken # outputValue
  let inputValue = pfield @"value" # ownInputResolved
  inputState <- pletFields @'["nullifierHashes"] oldState
  -- Allowed transitions given a redeemer:
  pmatch redeemer \case
    PDeposit c -> popaque $ unTermCont do
      let commit = pfield @"commitment" # c
      validateDeposit conf inputState outputState inputValue outputValue commit
    _ -> ptraceError "Not implemented"

validateDeposit ::
  ( PMemberFields PMixerDatum '["nullifierHashes"] s datum
  , PMemberFields PMixerConfig '["poolNominal"] s config
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
