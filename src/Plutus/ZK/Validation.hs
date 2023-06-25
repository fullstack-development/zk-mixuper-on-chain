{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.ZK.Validation where

import Plutus.Pairing (G1, GT, gAdd, gMul, gNeg, pairing)
import Plutus.ZK.Types (Fr, Proof (..), VerificationKey (..))
import PlutusTx.Prelude

{-# INLINEABLE validateProof #-}
validateProof :: VerificationKey -> [Fr] -> Proof -> Bool
validateProof vk input proof = mkVerifyProof vk input proof == one

{-# INLINEABLE mkVkX #-}
mkVkX :: [Fr] -> G1 -> [G1] -> G1
mkVkX input ic0 ic = foldl gAdd ic0 (zipWith gMul ic input)

{-# INLINEABLE mkVerifyProof #-}
mkVerifyProof :: VerificationKey -> [Fr] -> Proof -> GT
mkVerifyProof VerificationKey {..} input Proof {..} =
  pairing (gNeg proofA) proofB
    <> pairing vkAlfa1 vkBeta2
    <> pairing vkX vkGamma2
    <> pairing proofC vkDelta2
  where
    vkX = mkVkX input vkIC0 vkIC
