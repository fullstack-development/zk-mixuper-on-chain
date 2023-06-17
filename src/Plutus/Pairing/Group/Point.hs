{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- | Affine point arithmetic defining the group operation on an
 elliptic curve E(F), for some field F. In our case the field F is
 given as some type t with AdditiveGroup and MultiplicativeGroup instances.
-}
module Plutus.Pairing.Group.Point where

import Data.Aeson (FromJSON, ToJSON)
import Ext.PlutusTx.Numeric (Numeric (..))
import GHC.Generics (Generic)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

{- | Points on a curve over a field @a@ represented as either affine
 coordinates or as a point at infinity.
-}
data Point a
  = -- | Affine point
    Point a a
  | -- | Point at infinity
    Infinity
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINEABLE getCoordinates #-}
getCoordinates :: Point a -> [a]
getCoordinates (Point x y) = [x, y]
getCoordinates Infinity = []

PlutusTx.makeLift ''Point

PlutusTx.makeIsDataIndexed
  ''Point
  [('Point, 0), ('Infinity, 1)]

{-# INLINEABLE gAdd #-}
gAdd :: (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) => Point t -> Point t -> Point t
gAdd p Infinity = p
gAdd Infinity q = q
gAdd (Point x1 y1) (Point x2 y2)
  | x1 == x2 = Infinity
  | otherwise = Point x3 y3
  where
    l = (y1 - y2) * inv (x1 - x2)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1

{-# INLINEABLE gDouble #-}
gDouble :: (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) => Point t -> Point t
gDouble Infinity = Infinity
gDouble (Point x y)
  | y == zero = Infinity
  | otherwise = Point x' y'
  where
    xx = x * x
    l = (xx + xx + xx) * inv (y + y)
    x' = l * l - x - x
    y' = l * (x - x') - y

-- | Negation (flipping the y component)
{-# INLINEABLE gNeg #-}
gNeg ::
  (AdditiveGroup t) =>
  Point t ->
  Point t
gNeg Infinity = Infinity
gNeg (Point x y) = Point x (negate y)

-- | Multiplication by a scalar
{-# INLINEABLE gMul #-}
gMul ::
  (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) =>
  Point t ->
  Integer ->
  Point t
gMul pt n
  | n < 0 = traceError "gMul: negative scalar not supported"
  | n == 0 = Infinity
  | n == 1 = pt
  | even n = gMul (gDouble pt) (n `divide` 2)
  | otherwise = gAdd (gMul (gDouble pt) (n `divide` 2)) pt

instance (Eq a) => Eq (Point a) where
  {-# INLINEABLE (==) #-}
  (Point x y) == (Point a b) = x == a && y == b
  Infinity == Infinity = True
  _ == _ = False

instance (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) => Semigroup (Point t) where
  {-# INLINEABLE (<>) #-}
  (<>) p1 p2
    | p1 == p2 = gDouble p1
    | otherwise = gAdd p1 p2

instance (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) => Monoid (Point t) where
  {-# INLINEABLE mempty #-}
  mempty = Infinity

instance (Eq t, AdditiveGroup t, MultiplicativeSemigroup t, Group t) => Group (Point t) where
  {-# INLINEABLE inv #-}
  inv Infinity = Infinity
  inv (Point x y) = Point x (zero - y)
