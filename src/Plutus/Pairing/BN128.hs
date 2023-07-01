{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- | Parameters chosen for the pairing. The parameters chosen here
 correspond to the BN128 curve (aka CurveSNARK).

 > a = 0
 > b = 3
 > k = 12
 > t = 4965661367192848881
 > q = 21888242871839275222246405745257275088696311157297823662689037894645226208583
 > r = 21888242871839275222246405745257275088548364400416034343698204186575808495617
 > ξ = 9 + u
-}
module Plutus.Pairing.BN128 where

import qualified PlutusTx
import PlutusTx.Prelude (Integer)

-- | Elliptic curve coefficent
{-# INLINEABLE _b #-}
_b :: Integer
_b = 3

-- | Elliptic curve coefficent
{-# INLINEABLE _a #-}
_a :: Integer
_a = 0

-- | Embedding degree
{-# INLINEABLE _k #-}
_k :: Integer
_k = 12

-- | BN parameter that determines the prime
{-# INLINEABLE _t #-}
_t :: Integer
_t = 4965661367192848881

{- | Characteristic of the finite fields we work with
 _q = 36*_t^4 + 36*_t^3 + 24*_t^2 + 6*_t + 1
-}
{-# INLINEABLE _q #-}
_q :: Integer
_q = 21888242871839275222246405745257275088696311157297823662689037894645226208583

{- | Order of elliptic curve E(Fq) G1, and therefore also the characteristic
 of the prime field we choose our exponents from
 _r = 36*_t^4 + 36*_t^3 + 18*_t^2 + 6*_t + 1
-}
{-# INLINEABLE _r #-}
_r :: Integer
_r = 21888242871839275222246405745257275088548364400416034343698204186575808495617

{- | Parameter used to define the twisted curve over Fq, with xi =
 xi_a + xi_b * i
-}
{-# INLINEABLE _xiA #-}
_xiA :: Integer
_xiA = 9

{- | Parameter used to define the twisted curve over Fq, with xi =
 xi_a + xi_b * i
-}
{-# INLINEABLE _xiB #-}
_xiB :: Integer
_xiB = 1

-- | Quadratic nonresidue in Fq
{-# INLINEABLE _nqr #-}
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582

-- | BN254 curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: [Integer]
parameterBin =
  [ 1
  , 1
  , 0
  , 1
  , 0
  , 0
  , -1
  , 0
  , 1
  , 1
  , 0
  , 0
  , 0
  , -1
  , 0
  , 0
  , 1
  , 1
  , 0
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 0
  , -1
  , 0
  , 0
  , 1
  , 1
  , 1
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , 1
  , 0
  , 0
  , -1
  , 0
  , 1
  , 1
  , 0
  , 0
  , 1
  , 0
  , 0
  , -1
  , 1
  , 0
  , 0
  , -1
  , 0
  , 1
  , 0
  , 1
  , 0
  , 0
  , 0
  ]
{-# INLINEABLE parameterBin #-}

-- | BN254 curve parameter @t@ in hexadecimal.
parameterHex :: Integer
parameterHex = 0x44e992b44a6909f1
{-# INLINEABLE parameterHex #-}
