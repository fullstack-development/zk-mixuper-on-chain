module Plutarch.Pairing.Group.Fq where

import Ext.Plutarch.Rational (pgcdExt)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.BN128 (_nqr, _q)
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

newtype PFq (s :: S)
  = PFq (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)

instance DerivePlutusType PFq where
  type DPTStrat _ = PlutusTypeNewtype

-- instance PTryFrom PData PFq
instance PTryFrom PData (PAsData PFq)

instance PNum PFq where
  (#+) = pfqAdd
  (#-) = pfqSub
  (#*) = pfqMul
  pabs = phoistAcyclic $ plam id
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      1
  pfromInteger = pmkFq

pfq :: Term s PInteger -> Term s PFq
pfq = pcon . PFq

pmkFq :: Integer -> Term s PFq
pmkFq i = pcon . PFq . pconstant $ i `PlutusTx.modulo` Plutus._q

pfqAdd :: Term s PFq -> Term s PFq -> Term s PFq
pfqAdd at bt = P.do
  PFq a <- pmatch at
  PFq b <- pmatch bt
  pfq $ pmod # (a + b) # _q

pfqSub :: Term s PFq -> Term s PFq -> Term s PFq
pfqSub at bt = P.do
  PFq a <- pmatch at
  PFq b <- pmatch bt
  pfq $ pmod # (a - b) # _q

pfqMul :: Term s PFq -> Term s PFq -> Term s PFq
pfqMul at bt = P.do
  PFq a <- pmatch at
  PFq b <- pmatch bt
  pfq $ pmod # (a * b) # _q

-- | Multiplicative inverse
pfqInv :: Term s PFq -> Term s PFq
pfqInv t = P.do
  PFq a <- pmatch t
  PPair i _ <- pmatch $ pgcdExt # a # _q
  pfq $ pmod # i # _q

-- | Quadratic non-residue
pfqNqr :: Term s PFq
pfqNqr = pfq _nqr

instance PlutusTx.Semigroup (Term s PFq) where
  (<>) = pfqMul

instance PlutusTx.Monoid (Term s PFq) where
  mempty = 1

instance PlutusTx.Group (Term s PFq) where
  inv = pfqInv
