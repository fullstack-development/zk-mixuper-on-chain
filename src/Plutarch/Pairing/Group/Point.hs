module Plutarch.Pairing.Group.Point where

import Ext.Plutarch.Num (peven)
import Plutarch.Api.V2 (PTuple)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Prelude
import PlutusTx.Monoid (Group (..))

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

pgAdd :: (PIsData a, PNum a, Group (Term s a)) => Term s (PPoint a) -> Term s (PPoint a) -> Term s (PPoint a)
pgAdd pt qt = pmatch pt \case
  PInfinity _ -> qt
  PPoint pr -> pmatch qt \case
    PInfinity _ -> pt
    PPoint qr -> P.do
      p <- pletFields @'["x", "y"] pr
      q <- pletFields @'["x", "y"] qr
      pif (p.x #== q.x) pInfinity P.do
        l <- plet $ (p.y - q.y) * inv (p.x - q.x)
        x <- plet $ l * l - p.x - q.x
        y <- plet $ l * (p.x - x) - p.y
        pPoint x y

pgDouble :: (PIsData a, PNum a, Group (Term s a)) => Term s (PPoint a) -> Term s (PPoint a)
pgDouble pt = pmatch pt \case
  PInfinity _ -> pInfinity
  PPoint pr -> P.do
    p <- pletFields @'["x", "y"] pr
    pif (p.y #== pdata zero) pInfinity P.do
      xx <- plet $ p.x * p.x
      l <- plet $ (xx + xx + xx) * inv (p.y + p.y)
      x <- plet $ l * l - p.x - p.x
      y <- plet $ l * (p.x - x) - p.y
      pPoint x y
    where
      zero :: PNum a => Term s a
      zero = pfromInteger 0

-- | Negation (flipping the y component)
pgNeg :: (PIsData a, PNum a) => Term s (PPoint a) -> Term s (PPoint a)
pgNeg pt = pmatch pt \case
  PInfinity _ -> pInfinity
  PPoint pr -> P.do
    p <- pletFields @'["x", "y"] pr
    pPoint p.x (pnegate # p.y)

-- | Multiplication by a scalar
pgMul :: (PIsData a, PNum a, Group (Term s a)) => Term s (PPoint a :--> PInteger :--> PPoint a)
pgMul =
  pfix #$ plam \self pt n ->
    pif (n #< 0) (ptraceError "pgMul: negative scalar not supported") $
      pif (n #== 0) pInfinity $
        pif (n #== 1) pt $
          pif
            (peven # n)
            (self # pgDouble pt # (pdiv # n # 2))
            (pgAdd (self # pgDouble pt # (pdiv # n # 2)) pt)
