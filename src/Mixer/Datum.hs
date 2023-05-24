module Mixer.Datum where

import Plutarch.Api.V2 (PCurrencySymbol, PTokenName, PTuple)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

-- | A type for representing hash digests.
type PHash = PByteString

-- | A sha-256 digest of (nullifier <> secret)
type PCommitment = PHash

type Lovelace = PInteger

data MixerRedeemer (s :: S)
  = Deposit (Term s (PDataRecord '["commitment" := PCommitment]))
  | Withdraw (Term s (PDataRecord '[]))
  -- TODO(?) Close redeemer for pool operator / creator (fixed pkh)
  -- Close (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType MixerRedeemer where
  type DPTStrat _ = PlutusTypeData

-- | An asset class, identified by a CurrencySymbol and a TokenName. PAssetClass :: PType
type PAssetClass = PTuple PCurrencySymbol PTokenName

newtype MixerConfig (s :: S)
  = MixerConfig (Term s (PDataRecord '["protocolToken" := PAssetClass, "poolNominal" := Lovelace]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType MixerConfig where
  type DPTStrat _ = PlutusTypeData

newtype MixerDatum (s :: S)
  = MixerDatum (Term s (PDataRecord '["nullifierHashes" := PBuiltinList (PAsData PInteger)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData, PEq)

instance DerivePlutusType MixerDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData MixerDatum
instance PTryFrom PData (PAsData MixerDatum)
