module Mixer.Datum where

import Plutarch.Api.V2 (PCurrencySymbol, PTokenName, PTuple)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

-- | A type for representing hash digests.
type PHash = PByteString

-- | A sha-256 digest of (nullifier <> secret)
type PCommitment = PHash

type PLovelace = PInteger

data PMixerRedeemer (s :: S)
  = PDeposit (Term s (PDataRecord '["commitment" := PCommitment]))
  | PWithdraw (Term s (PDataRecord '[]))
  -- TODO(?) Close redeemer for pool operator / creator (fixed pkh)
  -- Close (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMixerRedeemer where
  type DPTStrat _ = PlutusTypeData

-- | An asset class, identified by a CurrencySymbol and a TokenName. PAssetClass :: PType
type PAssetClass = PTuple PCurrencySymbol PTokenName

newtype PMixerConfig (s :: S)
  = PMixerConfig (Term s (PDataRecord '["protocolToken" := PAssetClass, "poolNominal" := PLovelace]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PMixerConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMixerConfig
instance PTryFrom PData (PAsData PMixerConfig)

newtype PMixerDatum (s :: S)
  = PMixerDatum (Term s (PDataRecord '["nullifierHashes" := PBuiltinList (PAsData PInteger)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData, PEq)

instance DerivePlutusType PMixerDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMixerDatum
instance PTryFrom PData (PAsData PMixerDatum)
