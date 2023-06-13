{-# LANGUAGE TemplateHaskell #-}

module Mixer.Datum.Plutus where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import qualified PlutusTx

data MixerConfig = MixerConfig
  { protocolCurrency :: CurrencySymbol
  , depositTreeTokenName :: TokenName
  , vaultTokenName :: TokenName
  , nullifierStoreTokenName :: TokenName
  , poolNominal :: Integer
  }
  deriving stock (Generic, Show, Eq)

PlutusTx.makeLift ''MixerConfig

PlutusTx.makeIsDataIndexed
  ''MixerConfig
  [('MixerConfig, 0)]
