module Plutarch.Pairing.Group.Point where

import Ext.Plutarch.Num (peven)
import Plutarch.Api.V2 (PTuple)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.Group.Class (
  PGroup (..),
  PMonoid (..),
  PSemigroup (..),
 )
import Plutarch.Prelude
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

data PPoint (a :: PType) (s :: S)
  = PPoint (Term s (PDataRecord '["x" := a, "y" := a]))
  | PInfinity (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType (PPoint a) where
  type DPTStrat _ = PlutusTypeData

instance (PTryFrom PData a) => PTryFrom PData (PPoint a)

-- instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PPoint a))

-- data PPoint (a :: PType) (s :: S)
--   = PPoint (Term s (PTuple a a))
--   | PInfinity (Term s (PDataRecord '[]))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PIsData)

pInfinity :: Term s (PPoint a)
pInfinity = pcon $ PInfinity pdnil

pPoint :: (PIsData a) => Term s a -> Term s a -> Term s (PPoint a)
pPoint x y =
  pcon $
    PPoint $
      pdcons @"x"
        # pdata x
          #$ pdcons @"y"
        # pdata y #$ pdnil

pgAdd :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a) -> Term s (PPoint a) -> Term s (PPoint a)
pgAdd pt qt = pgAdd' # pt # qt

pgAdd' :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PPoint a :--> PPoint a)
pgAdd' = phoistAcyclic $ plam
  \pt qt -> pmatch pt \case
    PInfinity _ -> qt
    PPoint pr -> pmatch qt \case
      PInfinity _ -> pt
      PPoint qr -> P.do
        p <- pletFields @'["x", "y"] pr
        q <- pletFields @'["x", "y"] qr
        pif (p.x #== q.x) pInfinity P.do
          l <- plet $ (p.y - q.y) * pinv (p.x - q.x)
          x <- plet $ l * l - p.x - q.x
          y <- plet $ l * (p.x - x) - p.y
          pPoint x y

pgDouble :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a) -> Term s (PPoint a)
pgDouble pt = pgDouble' # pt

pgDouble' :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PPoint a)
pgDouble' = phoistAcyclic $ plam
  \pt -> pmatch pt \case
    PInfinity _ -> pInfinity
    PPoint pr -> P.do
      p <- pletFields @'["x", "y"] pr
      pif (p.y #== pdata 0) pInfinity P.do
        xx <- plet $ p.x * p.x
        l <- plet $ (xx + xx + xx) * pinv (p.y + p.y)
        x <- plet $ l * l - p.x - p.x
        y <- plet $ l * (p.x - x) - p.y
        pPoint x y

pgNeg :: (PIsData a, PNum a) => Term s (PPoint a) -> Term s (PPoint a)
pgNeg pt = pgNeg' # pt

-- | Negation (flipping the y component)
pgNeg' :: (PIsData a, PNum a) => Term s (PPoint a :--> PPoint a)
pgNeg' = phoistAcyclic $ plam
  \pt -> pmatch pt \case
    PInfinity _ -> pInfinity
    PPoint pr -> P.do
      p <- pletFields @'["x", "y"] pr
      pPoint p.x (pnegate # p.y)

-- | Multiplication by a scalar
pgMul :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PInteger :--> PPoint a)
pgMul = phoistAcyclic $
  plam $
    \pt n ->
      pif (n #< 0) (ptraceError "pgMul: negative scalar not supported") $
        pif (n #== 0) pInfinity $
          pif (n #== 1) pt (f # pt # n)
  where
    f :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PInteger :--> PPoint a)
    f =
      phoistAcyclic $
        pfix #$ plam \self x y -> P.do
          doubleP <- plet $ pgDouble x
          next <- plet $ pquot # y # 2
          pif (peven # y) (self # doubleP # next) $
            pif (y #== 1) x $
              g # doubleP # next # x
    g :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PInteger :--> PPoint a :--> PPoint a)
    g =
      phoistAcyclic $
        pfix #$ plam \self x y z -> P.do
          doubleP <- plet $ pgDouble x
          xz <- plet $ pgAdd x z
          next <- plet $ pquot # y # 2
          pif (peven # y) (self # doubleP # next # z) $
            pif (y #== 1) xz $
              self # doubleP # next # xz

instance (PIsData a, PNum a, PGroup a) => PSemigroup (PPoint a) where
  pappend p1 p2 = pif (p1 #== p2) (pgDouble p1) (pgAdd p1 p2)

instance (PIsData a, PNum a, PGroup a) => PMonoid (PPoint a) where
  pidentity = pInfinity

instance (PIsData a, PNum a, PGroup a) => PGroup (PPoint a) where
  pinv = pgNeg
