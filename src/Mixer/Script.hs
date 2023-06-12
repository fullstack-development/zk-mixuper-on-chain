module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (filterInputsByToken, inlineDatumFromOutput, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Mixer.Datum (PAssetClass, PWithdrawConfig, PWithdrawDatum (..), PWithdrawRedeemer (..))
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
  outputValue <- plet (ownOutput.value)
  -- Check protocol token:
  conf <- pletFields @'["protocolToken", "poolNominal"] config
  PUnit <- pmatch $ containsOneProtocolToken # conf.protocolToken # outputValue
  let inputValue = pfield @"value" # ownInputResolved
  inputState <- pletFields @'["nullifierHashes"] oldState
  -- Get deposit tree reference input:
  protocolToken <- pletFields @'["_0", "_1"] conf.protocolToken
  let inp = pgetOnlyOneOutputFromList #$ filterInputsByToken # protocolToken._0 # protocolToken._1 # info.referenceInputs
  -- Validate withdraw:
  popaque $
    unTermCont $
      validateWithdraw conf inputState outputState inputValue outputValue

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
    ( PAssetClass
        :--> SortedPositiveValue
        :--> PUnit
    )
containsOneProtocolToken = phoistAcyclic $
  plam $ \assetClass val -> unTermCont do
    protocolToken <- pletFieldsC @'["_0", "_1"] assetClass
    pguardC "Output should contain one protocol token" $ (pvalueOf # val # protocolToken._0 # protocolToken._1) #== 1
    pure $ pconstant ()
