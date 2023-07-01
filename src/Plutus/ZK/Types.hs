{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.ZK.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.Pairing.Group (G1, G2)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | Public input (exponent) represented as a number in field Fr
type Fr = Integer

data VerificationKey = VerificationKey
  { vkAlfa1 :: G1
  , vkBeta2 :: G2
  , vkGamma2 :: G2
  , vkDelta2 :: G2
  , vkIC0 :: G1
  , vkIC :: [G1]
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''VerificationKey

PlutusTx.makeIsDataIndexed
  ''VerificationKey
  [('VerificationKey, 0)]

data Proof = Proof
  { proofA :: G1
  , proofB :: G2
  , proofC :: G1
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''Proof

PlutusTx.makeIsDataIndexed
  ''Proof
  [('Proof, 0)]
