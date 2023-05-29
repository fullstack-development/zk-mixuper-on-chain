{-# LANGUAGE TemplateHaskell #-}

module Mixer.Datum.Plutus where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass)
import qualified PlutusTx

data MixerConfig = MixerConfig
  { protocolToken :: AssetClass
  , poolNominal :: Integer
  }
  deriving stock (Generic, Show, Eq)

PlutusTx.makeLift ''MixerConfig

PlutusTx.makeIsDataIndexed
  ''MixerConfig
  [('MixerConfig, 0)]