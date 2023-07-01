module Plutarch.Pairing.Group.Fq2 where

import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.Group.Class (
  PGroup (..),
  PMonoid (..),
  PSemigroup (..),
 )
import Plutarch.Pairing.Group.Fq (PFq (..), pfqInv, pfqNqr, pmkFq)
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

newtype PFq2 (s :: S)
  = PFq2 (Term s (PDataRecord '["x" := PFq, "y" := PFq]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PFq2 where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFq2
instance PTryFrom PData (PAsData PFq2)

instance PNum PFq2 where
  (#+) = pfq2Add
  (#-) = pfq2Sub
  (#*) = pfq2Mul
  pabs = phoistAcyclic $ plam id
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      1
  pfromInteger = pmkFq2

pFq2 :: Term s PFq -> Term s PFq -> Term s PFq2
pFq2 x y =
  pcon $
    PFq2 $
      pdcons @"x"
        # pdata x
          #$ pdcons @"y"
        # pdata y #$ pdnil

pmkFq2 :: Integer -> Term s PFq2
pmkFq2 i =
  pcon $
    PFq2 $
      pdcons @"x"
        # x
          #$ pdcons @"y"
        # y #$ pdnil
  where
    x = pdata (pmkFq i)
    y = pdata 0

pfq2Add :: Term s PFq2 -> Term s PFq2 -> Term s PFq2
pfq2Add at bt = pfq2Add' # at # bt

pfq2Add' :: Term s (PFq2 :--> PFq2 :--> PFq2)
pfq2Add' = phoistAcyclic $ plam
  \at bt -> P.do
    a <- pletFields @'["x", "y"] at
    b <- pletFields @'["x", "y"] bt
    pFq2 (a.x + b.x) (a.y + b.y)

pfq2Sub :: Term s PFq2 -> Term s PFq2 -> Term s PFq2
pfq2Sub at bt = pfq2Sub' # at # bt

pfq2Sub' :: Term s (PFq2 :--> PFq2 :--> PFq2)
pfq2Sub' = phoistAcyclic $ plam
  \at bt -> P.do
    a <- pletFields @'["x", "y"] at
    b <- pletFields @'["x", "y"] bt
    pFq2 (a.x - b.x) (a.y - b.y)

pfq2Mul :: Term s PFq2 -> Term s PFq2 -> Term s PFq2
pfq2Mul at bt = pfq2Mul' # at # bt

pfq2Mul' :: Term s (PFq2 :--> PFq2 :--> PFq2)
pfq2Mul' = phoistAcyclic $ plam
  \at bt -> P.do
    a <- pletFields @'["x", "y"] at
    b <- pletFields @'["x", "y"] bt
    xx <- plet (a.x #* b.x)
    yy <- plet (a.y #* b.y)
    pFq2 (yy * pfqNqr + xx) $ (a.x + a.y) * (b.x + b.y) - xx - yy

pfq2Inv :: Term s PFq2 -> Term s PFq2
pfq2Inv at = pfq2Inv' # at

-- | Multiplicative inverse
pfq2Inv' :: Term s (PFq2 :--> PFq2)
pfq2Inv' = phoistAcyclic $ plam
  \at -> P.do
    a <- pletFields @'["x", "y"] at
    t <- plet $ pfqInv (a.x * a.x - a.y * a.y * pfqNqr)
    pFq2 (a.x * t) $ pnegate # (a.y * t)

pfq2Conj :: Term s PFq2 -> Term s PFq2
pfq2Conj at = pfq2Conj' # at

-- | Conjugation
pfq2Conj' :: Term s (PFq2 :--> PFq2)
pfq2Conj' = phoistAcyclic $ plam
  \at -> P.do
    a <- pletFields @'["x", "y"] at
    pFq2 a.x (pnegate # a.y)

-- | Cubic non-residue in @Fq2@
pxi :: Term s PFq2
pxi = pFq2 (pmkFq Plutus._xiA) (pmkFq Plutus._xiB)

pfq2scalarMul :: Term s PFq -> Term s PFq2 -> Term s PFq2
pfq2scalarMul a bt = pfq2scalarMul' # a # bt

-- | Multiplication by a scalar in @Fq@
pfq2scalarMul' :: Term s (PFq :--> PFq2 :--> PFq2)
pfq2scalarMul' = phoistAcyclic $ plam
  \a bt -> P.do
    b <- pletFields @'["x", "y"] bt
    pFq2 (a * b.x) (a * b.y)

-- | Multiply by @xi@
pmulXiFq2 :: Term s (PFq2 :--> PFq2)
pmulXiFq2 = phoistAcyclic $ plam (#* pxi)

instance PlutusTx.Semigroup (Term s PFq2) where
  (<>) = pfq2Mul

instance PlutusTx.Monoid (Term s PFq2) where
  mempty = 1

instance PlutusTx.Group (Term s PFq2) where
  inv = pfq2Inv

instance PSemigroup PFq2 where
  pappend = pfq2Mul

instance PMonoid PFq2 where
  pidentity = 1

instance PGroup PFq2 where
  pinv = pfq2Inv
