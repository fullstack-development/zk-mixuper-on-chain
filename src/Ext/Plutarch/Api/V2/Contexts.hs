module Ext.Plutarch.Api.V2.Contexts where

import Plutarch.Api.V2
import Plutarch.Prelude

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"
  @
-}
pfindOwnInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

pgetContinuingOutputs :: Term s (PBuiltinList PTxOut :--> PTxOut :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \outputs ownInputResolved -> do
    let outAddr = pfield @"address" # ownInputResolved
    pfilter # (matches # outAddr) # outputs
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

pgetOnlyOneOutputFromList :: Term s (PBuiltinList PTxOut :--> PTxOut)
pgetOnlyOneOutputFromList = phoistAcyclic $
  plam $ \outputs ->
    pmatch outputs $ \case
      PNil -> ptraceError "No outputs found"
      PCons scriptTxOut rest -> do
        pmatch rest $ \case
          PNil -> scriptTxOut
          _ -> ptraceError "More than one output found"

inlineDatumFromOutput :: Term s (POutputDatum :--> PData)
inlineDatumFromOutput = phoistAcyclic $
  plam $ \scriptDatum ->
    pmatch scriptDatum $ \case
      POutputDatum d ->
        pmatch (pfield @"outputDatum" # d) $ \case
          PDatum datum -> datum
      _ -> ptraceError "Output should contain inlinable datum"