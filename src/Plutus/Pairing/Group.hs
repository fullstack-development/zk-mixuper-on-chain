{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Pairing.Group where

import Ext.PlutusTx.Numeric (Numeric (mkFromInteger), (^))
import Plutus.Pairing.BN128
import Plutus.Pairing.Group.Fq as Fq
import Plutus.Pairing.Group.Fq12 as Fq12
import Plutus.Pairing.Group.Fq2 as Fq2
import Plutus.Pairing.Group.Point
import PlutusTx.Prelude

{- | G1 is E(Fq) defined by y^2 = x^3 + b
 It is an additive cyclic group
-}
type G1 = Point Fq

{- | G2 is E'(Fq2) defined by y^2 = x^3 + b / xi
 It is an additive cyclic group
-}
type G2 = Point Fq2

{- | GT is subgroup of _r-th roots of unity of the multiplicative
 group of Fq12
-}
type GT = Fq12

{-# INLINEABLE isOnCurve #-}

-- | Test whether a value satisfies the corresponding curve equation
isOnCurve ::
  (Eq a, AdditiveSemigroup a, MultiplicativeMonoid a, Numeric a) =>
  Point a ->
  Bool
isOnCurve Infinity =
  True
isOnCurve (Point x y) =
  y ^ 2 == x ^ 3 + mkFromInteger _b

{- | Iterated frobenius morphisms on fields of characteristic _q,
 implemented naively
-}
frobeniusNaive :: (MultiplicativeMonoid a) => a -> a
frobeniusNaive a = a ^ _q
{-# INLINEABLE frobeniusNaive #-}
