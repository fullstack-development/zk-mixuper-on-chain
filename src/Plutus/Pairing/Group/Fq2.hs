{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Pairing.Group.Fq2 where

import Data.Aeson (FromJSON, ToJSON)
import Ext.PlutusTx.Numeric (
  Numeric (..),
  (^),
 )
import GHC.Generics (Generic)
import Plutus.Pairing.BN128
import Plutus.Pairing.Group.Fq as Fq
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | Quadratic extension of @Fq@ defined as @Fq[u]/x^2 + 1@
data Fq2
  = -- | Use @mkFq2@ instead of
    -- this contructor
    Fq2 {fq2x :: Fq, fq2y :: Fq}
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''Fq2

PlutusTx.makeIsDataIndexed
  ''Fq2
  [('Fq2, 0)]

instance Eq Fq2 where
  {-# INLINEABLE (==) #-}
  (Fq2 x y) == (Fq2 a b) = x == a && y == b

-- | @mkFq2 x y@ creates a value representing @x + y * u @
{-# INLINEABLE mkFq2 #-}
mkFq2 :: Fq -> Fq -> Fq2
mkFq2 = Fq2

{-# INLINEABLE fq2int #-}
fq2int :: Integer -> Fq2
fq2int n = Fq2 (mkFromInteger n) zero

instance Numeric Fq2 where
  {-# INLINEABLE mkFromInteger #-}
  mkFromInteger = fq2int

instance AdditiveSemigroup Fq2 where
  {-# INLINEABLE (+) #-}
  (+) = fq2Add

instance AdditiveMonoid Fq2 where
  {-# INLINEABLE zero #-}
  zero = fq2int 0

instance AdditiveGroup Fq2 where
  {-# INLINEABLE (-) #-}
  (-) = fq2Sub

instance MultiplicativeSemigroup Fq2 where
  {-# INLINEABLE (*) #-}
  (*) = fq2Mul

instance MultiplicativeMonoid Fq2 where
  {-# INLINEABLE one #-}
  one = fq2int 1

instance Semigroup Fq2 where
  {-# INLINEABLE (<>) #-}
  (<>) = (*)

instance Monoid Fq2 where
  {-# INLINEABLE mempty #-}
  mempty = one

instance Group Fq2 where
  {-# INLINEABLE inv #-}
  inv = fq2Inv

{-# INLINEABLE fq2Add #-}
fq2Add :: Fq2 -> Fq2 -> Fq2
fq2Add (Fq2 x y) (Fq2 a b) = Fq2 (x + a) (y + b)

{-# INLINEABLE fq2Sub #-}
fq2Sub :: Fq2 -> Fq2 -> Fq2
fq2Sub (Fq2 x y) (Fq2 a b) = Fq2 (x - a) (y - b)

{-# INLINEABLE fq2Mul #-}
fq2Mul :: Fq2 -> Fq2 -> Fq2
fq2Mul (Fq2 a0 a1) (Fq2 b0 b1) = Fq2 c0 c1
  where
    aa = a0 * b0
    bb = a1 * b1
    c0 = bb * fqNqr + aa
    c1 = (a0 + a1) * (b0 + b1) - aa - bb

-- | Multiplicative inverse
fq2Inv :: Fq2 -> Fq2
fq2Inv (Fq2 a0 a1) = Fq2 c0 c1
  where
    t = fqInv ((a0 ^ 2) - ((a1 ^ 2) * fqNqr))
    c0 = a0 * t
    c1 = negate (a1 * t)
{-# INLINEABLE fq2Inv #-}

-- | Conjugation
fq2Conj :: Fq2 -> Fq2
fq2Conj (Fq2 x y) = Fq2 x (negate y)
{-# INLINEABLE fq2Conj #-}

-- | Cubic non-residue in @Fq2@
xi :: Fq2
xi = Fq2 xiA xiB
  where
    xiA, xiB :: Fq
    xiA = Fq.mkFq _xiA
    xiB = Fq.mkFq _xiB
{-# INLINEABLE xi #-}

-- | Multiply by @xi@
mulXiFq2 :: Fq2 -> Fq2
mulXiFq2 = (* xi)
{-# INLINEABLE mulXiFq2 #-}

-- | Multiplication by a scalar in @Fq@
fq2scalarMul :: Fq -> Fq2 -> Fq2
fq2scalarMul a (Fq2 x y) = Fq2 (a * x) (a * y)
{-# INLINEABLE fq2scalarMul #-}
