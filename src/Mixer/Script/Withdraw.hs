module Mixer.Script.Withdraw where

import Ext.Plutarch.Api.V2.Contexts (filterInputsByToken, filterOutputsByToken, inlineDatumFromOutput, pAdaAmountPaidTo, pfindOwnInput, pgetContinuingOutputs, pgetOnlyOneOutputFromList)
import Ext.Plutarch.Api.V2.Value (SortedPositiveValue)
import Ext.Plutarch.ByteString (pbyteString2Integer)
import Mixer.Datum (PMixerDatum (PDepositTree, PVault), PWithdrawConfig, PWithdrawDatum (..), PWithdrawRedeemer (..))
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, padaSymbol, padaToken, plovelaceValueOf, pvalueOf)
import Plutarch.Api.V2 (
  PDatum,
  PMaybeData (..),
  PPubKeyHash (PPubKeyHash),
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.Builtin (pasInt, pforgetData)
import Plutarch.DataRepr (HRec, HRecOf, PMemberFields)
import Plutarch.Extra.Maybe (pfromDJust)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptryFromC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.ZK.Types (pProofFromData, pVerificationKeyFromData, pnormalizeFr)
import Plutarch.ZK.Validation (pvalidateProof, pvalidateProofPoints)

validateWithdraw ::
  ( PMemberFields PWithdrawDatum '["nullifierHashes"] s datum
  , PMemberFields PWithdrawConfig '["poolNominal", "vk"] s config
  , PMemberFields PWithdrawRedeemer '["publicInput", "proof"] s redeemer
  ) =>
  HRec config ->
  Term s PTxInfo ->
  HRec datum ->
  HRec datum ->
  Term s SortedPositiveValue ->
  Term s SortedPositiveValue ->
  Term s (PMaybeData (PAsData PInteger)) ->
  HRec redeemer ->
  TermCont s (Term s PUnit)
validateWithdraw conf txInfo inputState outputState inputValue outputValue currentRoot redeemer = do
  PDJust root <- pmatchC currentRoot
  let mtr = pforgetData $ pfield @"_0" # root
  publicInput <- pletFieldsC @'["treeRoot", "nullifierHash", "recipient", "relayer", "fee"] redeemer.publicInput
  treeRoot <- pletC $ pnormalizeFr publicInput.treeRoot
  nullifierHash <- pletC $ pnormalizeFr publicInput.nullifierHash
  fee <- pletC $ pnormalizeFr publicInput.fee
  pguardC "Merkle root is not current" $ (pasInt # mtr) #== treeRoot
  pguardC "Fee exceeds pool nominal" $ fee #< conf.poolNominal
  pguardC "Relayer should be submitting this tx" $ pelem # publicInput.relayer #$ pfield @"signatories" # txInfo
  pguardC "The note has been spent before" $ pnot #$ pelem # pdata nullifierHash # inputState.nullifierHashes
  pguardC "Incorrect nullifier hash list update" $ outputState.nullifierHashes #== (pcons # pdata nullifierHash # inputState.nullifierHashes)
  spentFromScript <- pletC $ pvalueOf # inputValue # padaSymbol # padaToken
  paidToScript <- pletC $ pvalueOf # outputValue # padaSymbol # padaToken
  paidToRecipient <- pletC $ pAdaAmountPaidTo # publicInput.recipient # txInfo
  pguardC "Withdrawn amount should be spent from script" $ spentFromScript - paidToScript #== conf.poolNominal
  pguardC "Recipient should receive deposit back" $ conf.poolNominal - fee #<= paidToRecipient
  -- TODO validate relayer was paid:
  -- withdrawnToRelayer = paidToRelayer - spentFromRelayer + txInfoFee txInfo
  proof <- pletC $ pProofFromData redeemer.proof
  PPubKeyHash piRecipient <- pmatchC publicInput.recipient
  recipient <- pletC $ pbyteString2Integer # 28 # piRecipient
  PPubKeyHash piRelayer <- pmatchC publicInput.relayer
  relayer <- pletC $ pbyteString2Integer # 28 # piRelayer
  let inputVec = pcons # treeRoot #$ pcons # nullifierHash #$ pcons # recipient #$ pcons # relayer #$ pcons # fee # pnil
  pguardC "Invalid ZK proof points" $ pvalidateProofPoints # proof
  pguardC "Invalid ZK proof" $ pvalidateProof # pVerificationKeyFromData conf.vk # inputVec # proof
  pure $ pconstant ()
