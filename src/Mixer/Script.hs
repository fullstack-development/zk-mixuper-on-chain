module Mixer.Script where

import Mixer.Datum (MixerConfig, MixerDatum (..), MixerRedeemer (..), PCommitment)
import Plutarch.Api.V2 (
  PDatum,
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
 )
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

validatorLogic ::
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
  PSpending _ <- pmatch ctx.purpose
  info <- pletFields @'["inputs", "outputs", "mint", "datums", "signatories"] $ ctx.txInfo
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
