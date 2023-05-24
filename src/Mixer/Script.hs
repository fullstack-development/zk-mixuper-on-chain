module Mixer.Script where

import Ext.Plutarch.Api.V2.Contexts (pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
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
validatorLogic = plam $ \conf (pfromData -> d) (pfromData -> r) ctx' -> P.do
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
    plet (pfield @"value" # ownInputResolved) $ \inputValue -> P.do
      pmatch r \case
        Deposit c -> unTermCont do
          let commit = pfield @"commitment" # c
          validateDeposit info commit
        _ -> ptraceError "Not implemented"

validateDeposit ::
  PMemberFields PTxInfo '["inputs", "outputs", "mint", "datums"] s txInfo =>
  HRec txInfo ->
  Term s PCommitment ->
  TermCont s (Term s PUnit)
validateDeposit _info commit = do
  -- Do checks with info fields here.
  pure $ pconstant ()
