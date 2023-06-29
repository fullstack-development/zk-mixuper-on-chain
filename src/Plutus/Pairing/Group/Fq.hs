{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- | Prime field with characteristic _q, over which the elliptic curve
 is defined and the other finite field extensions. First field in
 the tower:

   * Fq
   * Fq2 := Fq[u]/u^2 + 1
   * Fq6 := Fq2[v]/v^3 - (9 + u)
   * Fq12 := Fq6[w]/w^2 - v
-}
module Plutus.Pairing.Group.Fq where

import Data.Aeson (FromJSON, ToJSON)
import Ext.PlutusTx.Builtins (gcdExt)
import Ext.PlutusTx.Numeric (
  Numeric (..),
 )
import GHC.Generics (Generic)
import Plutus.Pairing.BN128
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | Prime field with characteristic @_q@
newtype Fq
  = -- | Use @mkFq@ instead of this
    -- constructor
    Fq Integer
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Fq

instance Eq Fq where
  {-# INLINEABLE (==) #-}
  (Fq x) == (Fq y) = x == y

instance Numeric Fq where
  {-# INLINEABLE mkFromInteger #-}
  mkFromInteger = mkFq

instance AdditiveSemigroup Fq where
  {-# INLINEABLE (+) #-}
  (+) = fqAdd

instance AdditiveMonoid Fq where
  {-# INLINEABLE zero #-}
  zero = Fq 0

instance AdditiveGroup Fq where
  {-# INLINEABLE (-) #-}
  (-) = fqSub

instance MultiplicativeSemigroup Fq where
  {-# INLINEABLE (*) #-}
  (*) = fqMul

instance MultiplicativeMonoid Fq where
  {-# INLINEABLE one #-}
  one = Fq 1

instance Semigroup Fq where
  {-# INLINEABLE (<>) #-}
  (<>) = (*)

instance Monoid Fq where
  {-# INLINEABLE mempty #-}
  mempty = one

instance Group Fq where
  {-# INLINEABLE inv #-}
  inv = fqInv

{-# INLINEABLE mkFq #-}
mkFq :: Integer -> Fq
mkFq i = Fq $ i `modulo` _q

{-# INLINEABLE fqAdd #-}
fqAdd :: Fq -> Fq -> Fq
fqAdd (Fq a) (Fq b) = Fq $ (a + b) `modulo` _q

{-# INLINEABLE fqSub #-}
fqSub :: Fq -> Fq -> Fq
fqSub (Fq a) (Fq b) = Fq $ (a - b) `modulo` _q

{-# INLINEABLE fqMul #-}
fqMul :: Fq -> Fq -> Fq
fqMul (Fq a) (Fq b) = Fq $ (a * b) `modulo` _q

-- | Multiplicative inverse
fqInv :: Fq -> Fq
fqInv (Fq a) =
  let (i, _, _) = gcdExt a _q
   in Fq $ i `modulo` _q
{-# INLINEABLE fqInv #-}

-- | Quadratic non-residue
fqNqr :: Fq
fqNqr = Fq _nqr
{-# INLINEABLE fqNqr #-}
