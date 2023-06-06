module Mixer.Script.Deposit where

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
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore.Data (Data)
import qualified Service.MerkleTree as T

validateDeposit ::
  ( PMemberFields PMixerDatum '["nullifierHashes", "merkleTreeState"] s datum
  , PMemberFields PMixerConfig '["poolNominal", "merkleTreeConfig"] s config
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
  inputTree <- pletC (pfield @"tree" # inputState.merkleTreeState)
  let nonEmptyLeafs = T.pnonEmptyLeafs # inputTree
  pguardC "Commitment has been submitted before" (pnot #$ pelem # commit # nonEmptyLeafs)
  let nextMerkleTreeState = T.pinsertMT # conf.merkleTreeConfig # commit # inputState.merkleTreeState
  pguardC "Incorrect Merkle Tree state update" (nextMerkleTreeState #== outputState.merkleTreeState)
  pure $ pconstant ()
