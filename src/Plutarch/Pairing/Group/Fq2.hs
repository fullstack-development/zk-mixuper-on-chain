module Plutarch.Pairing.Group.Fq2 where

import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.Group.Fq (PFq (..), pfqInv, pfqNqr, pmkFq)
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus

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
pfq2Add at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  pFq2 (a.x + b.x) (a.y + b.y)

pfq2Sub :: Term s PFq2 -> Term s PFq2 -> Term s PFq2
pfq2Sub at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  pFq2 (a.x - b.x) (a.y - b.y)

pfq2Mul :: Term s PFq2 -> Term s PFq2 -> Term s PFq2
pfq2Mul at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  xx <- plet (a.x #* b.x)
  yy <- plet (a.y #* b.y)
  pFq2 (yy * pfqNqr + xx) $ (a.x + a.y) * (b.x + b.y) - xx - yy

-- | Multiplicative inverse
pfq2Inv :: Term s PFq2 -> Term s PFq2
pfq2Inv at = P.do
  a <- pletFields @'["x", "y"] at
  t <- plet $ pfqInv (a.x * a.x - a.y * a.y * pfqNqr)
  pFq2 (a.x * t) $ pnegate # (a.y * t)

-- | Conjugation
pfq2Conj :: Term s PFq2 -> Term s PFq2
pfq2Conj at = P.do
  a <- pletFields @'["x", "y"] at
  pFq2 a.x (pnegate # a.y)

-- | Cubic non-residue in @Fq2@
pxi :: Term s PFq2
pxi = pFq2 (pmkFq Plutus._xiA) (pmkFq Plutus._xiB)

-- | Multiplication by a scalar in @Fq@
pfq2scalarMul :: Term s PFq -> Term s PFq2 -> Term s PFq2
pfq2scalarMul a bt = P.do
  b <- pletFields @'["x", "y"] bt
  pFq2 (a * b.x) (a * b.y)

-- | Multiply by @xi@
pmulXiFq2 :: Term s (PFq2 :--> PFq2)
pmulXiFq2 = phoistAcyclic $ plam (#* pxi)
