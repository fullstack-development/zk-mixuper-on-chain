module Plutarch.Pairing.BN128 where

import Plutarch.Prelude

-- | Elliptic curve coefficent
_b :: Term s PInteger
_b = pconstant 3

-- | Elliptic curve coefficent
_a :: Term s PInteger
_a = pconstant 0

-- | Embedding degree
_k :: Term s PInteger
_k = pconstant 12

-- | BN parameter that determines the prime
_t :: Term s PInteger
_t = pconstant 4965661367192848881

{- | Characteristic of the finite fields we work with
 _q = 36*_t^4 + 36*_t^3 + 24*_t^2 + 6*_t + 1
-}
_q :: Term s PInteger
_q = pconstant 21888242871839275222246405745257275088696311157297823662689037894645226208583

{- | Order of elliptic curve E(Fq) G1, and therefore also the characteristic
 of the prime field we choose our exponents from
 _r = 36*_t^4 + 36*_t^3 + 18*_t^2 + 6*_t + 1
-}
_r :: Term s PInteger
_r = pconstant 21888242871839275222246405745257275088548364400416034343698204186575808495617

{- | Parameter used to define the twisted curve over Fq, with xi =
 xi_a + xi_b * i
-}
_xiA :: Term s PInteger
_xiA = pconstant 9

{- | Parameter used to define the twisted curve over Fq, with xi =
 xi_a + xi_b * i
-}
_xiB :: Term s PInteger
_xiB = pconstant 1

-- | Quadratic nonresidue in Fq
_nqr :: Term s PInteger
_nqr = pconstant 21888242871839275222246405745257275088696311157297823662689037894645226208582

-- | BN254 curve parameter @t@ in hexadecimal.
parameterHex :: Term s PInteger
parameterHex = pconstant 0x44e992b44a6909f1

-- | BN254 curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: Term s (PList PInteger)
parameterBin =
  pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant (-1)
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 1
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
      #$ pcons
    # pconstant 0
    # pnil
