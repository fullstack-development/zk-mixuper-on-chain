module Plutarch.Pairing.Group.Point where

import Ext.Plutarch.Num (peven)
import Plutarch.Api.V2 (PTuple)
import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.BN128 (_a)
import Plutarch.Pairing.Group.Class (
  PGroup (..),
  PMonoid (..),
  PSemigroup (..),
 )
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus
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
    \pt n -> pfromJ' #$ pmulJ # ptoJ pt # n

-- | Multiplication by a scalar
pgMulHeavy :: (PIsData a, PNum a, PGroup a) => Term s (PPoint a :--> PInteger :--> PPoint a)
pgMulHeavy = phoistAcyclic $
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

--------------------- Jacobian coordinates -----------------------------
newtype PPointJ (a :: PType) (s :: S)
  = PPointJ (Term s (PDataRecord '["x" := a, "y" := a, "z" := a]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType (PPointJ a) where
  type DPTStrat _ = PlutusTypeData

instance (PTryFrom PData a) => PTryFrom PData (PPointJ a)

-- instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PPointJ a))

pPointJ :: (PIsData a) => Term s a -> Term s a -> Term s a -> Term s (PPointJ a)
pPointJ x y z =
  pcon $
    PPointJ $
      pdcons @"x"
        # pdata x
          #$ pdcons @"y"
        # pdata y
          #$ pdcons @"z"
        # pdata z
        # pdnil

ptoJ :: (PIsData a, PNum a) => Term s (PPoint a) -> Term s (PPointJ a)
ptoJ pt = pmatch pt \case
  PInfinity _ -> pPointJ 1 1 0
  PPoint pr -> P.do
    p <- pletFields @'["x", "y"] pr
    pPointJ p.x p.y 1

pfromJ :: (PIsData a, PNum a, PGroup a) => Term s (PPointJ a) -> Term s (PPoint a)
pfromJ pt = pfromJ' # pt

pfromJ' :: (PIsData a, PNum a, PGroup a) => Term s (PPointJ a :--> PPoint a)
pfromJ' = phoistAcyclic $
  plam $
    \pt -> P.do
      p <- pletFields @'["x", "y", "z"] pt
      pif (p.z #== pdata 0) pInfinity P.do
        zz <- plet $ p.z * p.z
        pPoint (p.x * pinv zz) (p.y * pinv (p.z * zz))

pinvJ :: (PIsData a, PNum a) => Term s (PPointJ a :--> PPointJ a)
pinvJ = phoistAcyclic $
  plam $
    \pt -> P.do
      p <- pletFields @'["x", "y", "z"] pt
      pPointJ p.x (pnegate # p.y) p.z

pdoubleJ :: forall s a. (PIsData a, PNum a) => Term s (PPointJ a :--> PPointJ a)
pdoubleJ = phoistAcyclic $
  plam $
    \pt -> P.do
      p <- pletFields @'["x", "y", "z"] pt
      one <- plet $ pfromInteger 1
      a <- plet $ pfromInteger Plutus._a
      two <- plet $ one + one
      three <- plet $ two + one
      eight <- plet $ three + three + two
      xx <- plet $ p.x * p.x
      yy <- plet $ p.y * p.y
      yyyy <- plet $ yy * yy
      zz <- plet $ p.z * p.z
      xy <- plet $ p.x + yy
      yz <- plet $ p.y + p.z
      s <- plet $ two * (xy * xy - xx - yyyy)
      m <- plet $ three * xx + a * zz * zz
      t <- plet $ m * m - two * s
      y3 <- plet $ m * (s - t) - eight * yyyy
      z3 <- plet $ yz * yz - yy - zz
      pPointJ t y3 z3

paddJ :: forall s a. (PIsData a, PNum a) => Term s (PPointJ a :--> PPointJ a :--> PPointJ a)
paddJ = phoistAcyclic $
  plam $
    \pt qt -> P.do
      p <- pletFields @'["x", "y", "z"] pt
      q <- pletFields @'["x", "y", "z"] qt
      pif (p.z #== pdata 0) (pPointJ q.x q.y q.z) $
        pif (q.z #== pdata 0) (pPointJ p.x p.y p.z) $
          pif (p.x #== q.x) (pPointJ 1 1 0) P.do
            one <- plet $ pfromInteger 1
            two <- plet $ one + one
            z1z1 <- plet $ p.z * p.z
            z2z2 <- plet $ q.z * q.z
            z1z2 <- plet $ p.z + q.z
            u1 <- plet $ p.x * z2z2
            u2 <- plet $ q.x * z1z1
            s1 <- plet $ p.y * q.z * z2z2
            s2 <- plet $ q.y * p.z * z1z1
            h <- plet $ u2 - u1
            h2 <- plet $ two * h
            i <- plet $ h2 * h2
            j <- plet $ h * i
            r <- plet $ two * (s2 - s1)
            v <- plet $ u1 * i
            x3 <- plet $ r * r - j - two * v
            y3 <- plet $ r * (v - x3) - two * s1 * j
            z3 <- plet $ (z1z2 * z1z2 - z1z1 - z2z2) * h
            pPointJ x3 y3 z3

pmulJ :: forall s a. (PIsData a, PNum a) => Term s (PPointJ a :--> PInteger :--> PPointJ a)
pmulJ =
  phoistAcyclic $
    pfix #$ plam \self pt n ->
      pif (n #< 0) (pinvJ #$ self # pt # (pnegate # n)) $
        pif (n #== 0) (pPointJ 1 1 0) $
          pif (n #== 1) pt P.do
            p' <- plet $ pmulJ # (pdoubleJ # pt) # (pdiv # n # 2)
            pif (peven # n) p' (paddJ # pt # p')
