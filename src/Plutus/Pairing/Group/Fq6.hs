{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Pairing.Group.Fq6 where

import Data.Aeson (FromJSON, ToJSON)
import Ext.PlutusTx.Numeric (
  Numeric (..),
  (^),
 )
import GHC.Generics (Generic)
import Plutus.Pairing.Group.Fq as Fq
import Plutus.Pairing.Group.Fq2 as Fq2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | Field extension defined as Fq2[v]/v^3 - (9 + u)
data Fq6 = Fq6
  { fq6x :: Fq2
  , fq6y :: Fq2
  , fq6z :: Fq2
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''Fq6

PlutusTx.makeIsDataIndexed
  ''Fq6
  [('Fq6, 0)]

instance Eq Fq6 where
  {-# INLINEABLE (==) #-}
  (Fq6 x y z) == (Fq6 a b c) = x == a && y == b && z == c

-- | @mkFq6 x y@ creates a Fq6 value
{-# INLINEABLE mkFq6 #-}
mkFq6 :: Fq2 -> Fq2 -> Fq2 -> Fq6
mkFq6 = Fq6

{-# INLINEABLE fq6int #-}
fq6int :: Integer -> Fq6
fq6int n = Fq6 (mkFromInteger n) zero zero

instance Numeric Fq6 where
  {-# INLINEABLE mkFromInteger #-}
  mkFromInteger = fq6int

instance AdditiveSemigroup Fq6 where
  {-# INLINEABLE (+) #-}
  (+) = fq6Add

instance AdditiveMonoid Fq6 where
  {-# INLINEABLE zero #-}
  zero = Fq6 zero zero zero

instance AdditiveGroup Fq6 where
  {-# INLINEABLE (-) #-}
  (-) = fq6Sub

instance MultiplicativeSemigroup Fq6 where
  {-# INLINEABLE (*) #-}
  (*) = fq6Mul

instance MultiplicativeMonoid Fq6 where
  {-# INLINEABLE one #-}
  one = Fq6 one zero zero

instance Semigroup Fq6 where
  {-# INLINEABLE (<>) #-}
  (<>) = (*)

instance Monoid Fq6 where
  {-# INLINEABLE mempty #-}
  mempty = one

instance Group Fq6 where
  {-# INLINEABLE inv #-}
  inv = fq6Inv

{-# INLINEABLE fq6Add #-}
fq6Add :: Fq6 -> Fq6 -> Fq6
fq6Add (Fq6 x y z) (Fq6 a b c) = Fq6 (x + a) (y + b) (z + c)

{-# INLINEABLE fq6Sub #-}
fq6Sub :: Fq6 -> Fq6 -> Fq6
fq6Sub (Fq6 x y z) (Fq6 a b c) = Fq6 (x - a) (y - b) (z - c)

{-# INLINEABLE fq6Mul #-}
fq6Mul :: Fq6 -> Fq6 -> Fq6
fq6Mul (Fq6 ax ay az) (Fq6 bx by bz) = Fq6 cx cy cz
  where
    xx = ax * bx
    yy = ay * by
    zz = az * bz
    cx = Fq2.mulXiFq2 ((ay + az) * (by + bz) - yy - zz) + xx
    cy = ((ax + ay) * (bx + by)) - xx - yy + Fq2.mulXiFq2 zz
    cz = ((ax + az) * (bx + bz)) - xx + yy - zz

-- | Multiplicative inverse
fq6Inv :: Fq6 -> Fq6
fq6Inv (Fq6 ax ay az) = Fq6 (t * c0) (t * c1) (t * c2)
  where
    c0 = ax ^ 2 - ay * az * Fq2.xi
    c1 = az ^ 2 * Fq2.xi - ax * ay
    c2 = ay ^ 2 - ax * az
    t = Fq2.fq2Inv ((az * c1 + ay * c2) * Fq2.xi + ax * c0)
{-# INLINEABLE fq6Inv #-}

{- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder
 coefficients
-}
mulXiFq6 :: Fq6 -> Fq6
mulXiFq6 (Fq6 x y z) = Fq6 (z * Fq2.xi) x y
{-# INLINEABLE mulXiFq6 #-}
