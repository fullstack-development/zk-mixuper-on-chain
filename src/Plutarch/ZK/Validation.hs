module Plutarch.ZK.Validation where

import Plutarch.List (pconvertLists, pfoldl')
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing (
  PG1,
  PGT,
  pgAdd',
  pgMul,
  pgNeg,
  ppairing,
 )
import Plutarch.Prelude
import Plutarch.ZK.Types (PFr, PProof (..), PVerificationKey (..))

pvalidateProof :: Term s (PVerificationKey :--> PList PFr :--> PProof :--> PBool)
pvalidateProof =
  phoistAcyclic $
    plam \vk input proof -> pmkVerifyProof # vk # input # proof #== 1

-- TODO optimize away ppairing # vk.alfa1 # vk.beta2
pmkVerifyProof :: Term s (PVerificationKey :--> PList PFr :--> PProof :--> PGT)
pmkVerifyProof =
  phoistAcyclic $
    plam \vk input proof -> P.do
      PProof a b c <- pmatch proof
      PVerificationKey alfa1 beta2 gamma2 delta2 ic0 ic <- pmatch vk
      vkX <- plet $ pmkVkX # input # ic0 # ic
      ppairing # pgNeg a # b
        * ppairing # alfa1 # beta2
        * ppairing # vkX # gamma2
        * ppairing # c # delta2

pmkVkX :: Term s (PList PFr :--> PG1 :--> PList PG1 :--> PG1)
pmkVkX =
  phoistAcyclic $
    plam \input ic0 ic ->
      pfoldl # pgAdd' # ic0 # (pzipWith # pgMul # ic # input)
