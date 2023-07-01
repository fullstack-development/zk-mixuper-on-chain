module Plutarch.ZK.Types where

import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Pairing.BN128 (_r)
import Plutarch.Pairing.Group (
  PG1,
  PG1Data,
  PG2,
  PG2Data,
  pG1fromData,
  pG1fromDataTrusted,
  pG2fromData,
  pG2fromDataTrusted,
 )
import Plutarch.Prelude

-- | Public input (exponent) represented as a number in field Fr
type PFr = PInteger

pnormalizeFr :: Term s PFr -> Term s PFr
pnormalizeFr f = pmod # f # _r

newtype PVerificationKeyData (s :: S)
  = PVerificationKeyData (Term s (PDataRecord '["alfa1" := PG1Data, "beta2" := PG2Data, "gamma2" := PG2Data, "delta2" := PG2Data, "ic0" := PG1Data, "ic" := PBuiltinList (PAsData PG1Data)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PDataFields, PIsData)

instance DerivePlutusType PVerificationKeyData where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVerificationKeyData
instance PTryFrom PData (PAsData PVerificationKeyData)

data PVerificationKey (s :: S)
  = PVerificationKey (Term s PG1) (Term s PG2) (Term s PG2) (Term s PG2) (Term s PG1) (Term s (PList PG1))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PVerificationKey where
  type DPTStrat _ = PlutusTypeScott

pVerificationKeyFromData :: Term s PVerificationKeyData -> Term s PVerificationKey
pVerificationKeyFromData vkt = P.do
  vk <- pletFields @'["alfa1", "beta2", "gamma2", "delta2", "ic0", "ic"] vkt
  pcon $
    PVerificationKey
      (pG1fromDataTrusted vk.alfa1)
      (pG2fromDataTrusted vk.beta2)
      (pG2fromDataTrusted vk.gamma2)
      (pG2fromDataTrusted vk.delta2)
      (pG1fromDataTrusted vk.ic0)
      (mkIC vk.ic)
  where
    mkIC :: Term s (PBuiltinList (PAsData PG1Data)) -> Term s (PList PG1)
    mkIC l = pfoldr # consG1 # pnil # l

    consG1 :: Term s1 (PAsData PG1Data :--> PList PG1 :--> PList PG1)
    consG1 = phoistAcyclic $
      plam $
        \x acc -> pcons # pG1fromDataTrusted (pfromData x) # acc

newtype PProofData (s :: S)
  = PProofData (Term s (PDataRecord '["a" := PG1Data, "b" := PG2Data, "c" := PG1Data]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PDataFields, PIsData)

instance DerivePlutusType PProofData where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProofData
instance PTryFrom PData (PAsData PProofData)

data PProof (s :: S)
  = PProof (Term s PG1) (Term s PG2) (Term s PG1)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PProof where
  type DPTStrat _ = PlutusTypeScott

pProofFromData :: Term s PProofData -> Term s PProof
pProofFromData prooft = P.do
  proof <- pletFields @'["a", "b", "c"] prooft
  pcon $
    PProof
      (pG1fromData proof.a)
      (pG2fromData proof.b)
      (pG1fromData proof.c)
