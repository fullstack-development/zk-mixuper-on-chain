module Plutarch.ZK.Example where

import Data.Text (Text, pack)
import Ext.Plutarch.Extra.Run (evalWithArgsT)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.Script (Script (..), serialiseScript)
import Plutarch.ZK.Types (
  PFr,
  PProofData,
  PVerificationKeyData,
  pProofFromData,
  pVerificationKeyFromData,
 )
import Plutarch.ZK.Validation (pvalidateProof)
import Plutus.ZK.Example (input, proof, verificationKey)
import PlutusCore.Data (Data)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusLedgerApi.V2 (toData)

test :: Either Text (Script, ExBudget, [Text])
test = evalWithArgsT prunValidateProof [pverificationKey, pinput, pproof]

prunValidateProof :: Term s (PData :--> PData :--> PData :--> PBool)
prunValidateProof = phoistAcyclic $
  plam \vkpd inputpd proofpd -> P.do
    (vkd, _) <- ptryFrom @PVerificationKeyData vkpd
    (inputd, _) <- ptryFrom @(PAsData (PBuiltinList (PAsData PFr))) inputpd
    (proofd, _) <- ptryFrom @PProofData proofpd
    vk <- plet $ pVerificationKeyFromData vkd
    input <- plet $ pInputFromData inputd
    proof <- plet $ pProofFromData proofd
    pvalidateProof # vk # input # proof
  where
    pInputFromData :: Term s (PAsData (PBuiltinList (PAsData PFr))) -> Term s (PList PFr)
    pInputFromData (pfromData -> xs) = pfoldr # consFr # pnil # xs

    consFr :: Term s (PAsData PFr :--> PList PFr :--> PList PFr)
    consFr = phoistAcyclic $
      plam $
        \x acc -> pcons # pfromData x # acc

pverificationKey :: Data
pverificationKey = toData verificationKey

pinput :: Data
pinput = toData input

pproof :: Data
pproof = toData proof
