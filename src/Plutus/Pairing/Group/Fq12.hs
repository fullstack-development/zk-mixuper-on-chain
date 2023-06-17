{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Pairing.Group.Fq12 where

import Data.Aeson (FromJSON, ToJSON)
import Ext.PlutusTx.List (replicate)
import Ext.PlutusTx.Numeric (
  Numeric (..),
  (^),
 )
import GHC.Generics (Generic)
import Plutus.Pairing.BN128
import Plutus.Pairing.Group.Fq as Fq
import Plutus.Pairing.Group.Fq2 as Fq2
import Plutus.Pairing.Group.Fq6 as Fq6
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | Field extension defined as Fq6[w]/w^2 - v
data Fq12
  = -- | Use @new@ instead
    -- of this constructor
    Fq12 {fq12x :: Fq6, fq12y :: Fq6}
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''Fq12

PlutusTx.makeIsDataIndexed
  ''Fq12
  [('Fq12, 0)]

instance Eq Fq12 where
  {-# INLINEABLE (==) #-}
  (Fq12 x y) == (Fq12 a b) = x == a && y == b

{-# INLINEABLE mkFq12 #-}

{- | Create a new value in @Fq12@ by providing a list of twelve
 coefficients in @Fq@, should be used instead of the @Fq12@
 constructor.
-}
mkFq12 :: [Fq] -> Fq12
mkFq12 [a, b, c, d, e, f, g, h, i, j, k, l] =
  Fq12
    (Fq6.mkFq6 (Fq2.mkFq2 a b) (Fq2.mkFq2 c d) (Fq2.mkFq2 e f))
    (Fq6.mkFq6 (Fq2.mkFq2 g h) (Fq2.mkFq2 i j) (Fq2.mkFq2 k l))
mkFq12 _ = traceError "Invalid arguments to fq12"

{-# INLINEABLE fq12int #-}
fq12int :: Integer -> Fq12
fq12int n = mkFq12 (mkFromInteger n : replicate 11 zero)

instance Numeric Fq12 where
  {-# INLINEABLE mkFromInteger #-}
  mkFromInteger = fq12int

instance AdditiveSemigroup Fq12 where
  {-# INLINEABLE (+) #-}
  (+) = fq12Add

instance AdditiveMonoid Fq12 where
  {-# INLINEABLE zero #-}
  zero = fq12int 0

instance AdditiveGroup Fq12 where
  {-# INLINEABLE (-) #-}
  (-) = fq12Sub

instance MultiplicativeSemigroup Fq12 where
  {-# INLINEABLE (*) #-}
  (*) = fq12Mul

instance MultiplicativeMonoid Fq12 where
  {-# INLINEABLE one #-}
  one = fq12int 1

instance Semigroup Fq12 where
  {-# INLINEABLE (<>) #-}
  (<>) = (*)

instance Monoid Fq12 where
  {-# INLINEABLE mempty #-}
  mempty = one

instance Group Fq12 where
  {-# INLINEABLE inv #-}
  inv = fq12Inv

{-# INLINEABLE fq12Add #-}
fq12Add :: Fq12 -> Fq12 -> Fq12
fq12Add (Fq12 x y) (Fq12 a b) = Fq12 (x + a) (y + b)

{-# INLINEABLE fq12Sub #-}
fq12Sub :: Fq12 -> Fq12 -> Fq12
fq12Sub (Fq12 x y) (Fq12 a b) = Fq12 (x - a) (y - b)

{-# INLINEABLE fq12Mul #-}
fq12Mul :: Fq12 -> Fq12 -> Fq12
fq12Mul (Fq12 x y) (Fq12 a b) = Fq12 (Fq6.mulXiFq6 bb + aa) ((x + y) * (a + b) - aa - bb)
  where
    aa = x * a
    bb = y * b

{-# INLINEABLE fq12Inv #-}

-- | Multiplicative inverse
fq12Inv :: Fq12 -> Fq12
fq12Inv (Fq12 a b) = Fq12 (a * t) (negate (b * t))
  where
    t = Fq6.fq6Inv (a ^ 2 - Fq6.mulXiFq6 (b ^ 2))

{-# INLINEABLE fq12Conj #-}

-- | Conjugation
fq12Conj :: Fq12 -> Fq12
fq12Conj (Fq12 x y) = Fq12 x (negate y)

-- | Iterated Frobenius automorphism
{-# INLINEABLE fq12frobenius #-}
fq12frobenius :: Integer -> Fq12 -> Fq12
fq12frobenius i a
  | i == 0 = a
  | i == 1 = fastFrobenius1 a
  | i > 1 =
      let prev = fq12frobenius (i - 1) a
       in fastFrobenius1 prev
  | otherwise = traceError "fq12frobenius not defined for negative values of i"

{-# INLINEABLE fastFrobenius1 #-}
fastFrobenius1 :: Fq12 -> Fq12
fastFrobenius1 (Fq12 (Fq6.Fq6 x0 x1 x2) (Fq6.Fq6 y0 y1 y2)) =
  let
    t1 = Fq2.fq2Conj x0
    t2 = Fq2.fq2Conj y0
    t3 = Fq2.fq2Conj x1
    t4 = Fq2.fq2Conj y1
    t5 = Fq2.fq2Conj x2
    t6 = Fq2.fq2Conj y2
    gamma1 :: Integer -> Fq2.Fq2
    gamma1 i = Fq2.xi ^ ((i * (_q - 1)) `divide` 6)
    t11 = t1
    t21 = t2 * gamma1 1
    t31 = t3 * gamma1 2
    t41 = t4 * gamma1 3
    t51 = t5 * gamma1 4
    t61 = t6 * gamma1 5
    c0 = Fq6 t11 t31 t51
    c1 = Fq6 t21 t41 t61
   in
    Fq12 c0 c1

{- | Unitary exponentiation @^@.

 Exponentiation of a unitary element @x@ to an arbitrary integer @n@
 in a specified cyclotomic subgroup.
-}
{-# INLINE powUnitary #-}
powUnitary :: Fq12 -> Integer -> Fq12
powUnitary x n
  | n < 0 = fq12Conj x ^ negate n
  | otherwise = x ^ n
