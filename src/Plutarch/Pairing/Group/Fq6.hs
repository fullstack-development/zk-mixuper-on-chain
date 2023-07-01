module Plutarch.Pairing.Group.Fq6 where

import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.Group.Fq2 (
  PFq2,
  pfq2Inv,
  pmkFq2,
  pmulXiFq2,
  pxi,
 )
import Plutarch.Prelude
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

newtype PFq6 (s :: S)
  = PFq6 (Term s (PDataRecord '["x" := PFq2, "y" := PFq2, "z" := PFq2]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PFq6 where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFq6
instance PTryFrom PData (PAsData PFq6)

instance PNum PFq6 where
  (#+) = pfq6Add
  (#-) = pfq6Sub
  (#*) = pfq6Mul
  pabs = phoistAcyclic $ plam id
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      1
  pfromInteger = pmkFq6

pFq6 :: Term s PFq2 -> Term s PFq2 -> Term s PFq2 -> Term s PFq6
pFq6 x y z =
  pcon $
    PFq6 $
      pdcons @"x"
        # pdata x
          #$ pdcons @"y"
        # pdata y
          #$ pdcons @"z"
        # pdata z
          #$ pdnil

pmkFq6 :: Integer -> Term s PFq6
pmkFq6 i =
  pcon $
    PFq6 $
      pdcons @"x"
        # x
          #$ pdcons @"y"
        # zero
          #$ pdcons @"z"
        # zero
          #$ pdnil
  where
    x = pdata (pmkFq2 i)
    zero = pdata 0

pfq6Add :: Term s PFq6 -> Term s PFq6 -> Term s PFq6
pfq6Add at bt = P.do
  a <- pletFields @'["x", "y", "z"] at
  b <- pletFields @'["x", "y", "z"] bt
  pFq6 (a.x + b.x) (a.y + b.y) (a.z + b.z)

pfq6Sub :: Term s PFq6 -> Term s PFq6 -> Term s PFq6
pfq6Sub at bt = P.do
  a <- pletFields @'["x", "y", "z"] at
  b <- pletFields @'["x", "y", "z"] bt
  pFq6 (a.x - b.x) (a.y - b.y) (a.z - b.z)

pfq6Mul :: Term s PFq6 -> Term s PFq6 -> Term s PFq6
pfq6Mul at bt = P.do
  a <- pletFields @'["x", "y", "z"] at
  b <- pletFields @'["x", "y", "z"] bt
  xx <- plet (a.x * b.x)
  yy <- plet (a.y * b.y)
  zz <- plet (a.z * b.z)
  cx <- plet $ (pmulXiFq2 # ((a.y + a.z) * (b.y + b.z) - yy - zz)) + xx
  cy <- plet $ ((a.x + a.y) * (b.x + b.y)) - xx - yy + (pmulXiFq2 # zz)
  cz <- plet $ ((a.x + a.z) * (b.x + b.z)) - xx + yy - zz
  pFq6 cx cy cz

-- | Multiplicative inverse
pfq6Inv :: Term s PFq6 -> Term s PFq6
pfq6Inv at = P.do
  a <- pletFields @'["x", "y", "z"] at
  c0 <- plet $ a.x * a.x - a.y * a.z * pxi
  c1 <- plet $ a.z * a.z * pxi - a.x * a.y
  c2 <- plet $ a.y * a.y - a.x * a.z
  t <- plet $ pfq2Inv ((a.z * c1 + a.y * c2) * pxi + a.x * c0)
  pFq6 (t * c0) (t * c1) (t * c2)

{- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder
 coefficients
-}
pmulXiFq6 :: Term s (PFq6 :--> PFq6)
pmulXiFq6 = phoistAcyclic $ plam \at -> P.do
  a <- pletFields @'["x", "y", "z"] at
  pFq6 (a.z * pxi) a.x a.y

instance PlutusTx.Semigroup (Term s PFq6) where
  (<>) = pfq6Mul

instance PlutusTx.Monoid (Term s PFq6) where
  mempty = 1

instance PlutusTx.Group (Term s PFq6) where
  inv = pfq6Inv
