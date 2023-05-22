module Mixer.Datum where

import Plutarch.Prelude

-- | A type for representing hash digests.
type Hash = PByteString

-- | A sha-256 digest of (nullifier <> secret)
type Commitment = Hash

data MixerRedeemer (s :: S)
  = Deposit (Term s (PDataRecord '["commitment" := Commitment]))
  | Withdraw (Term s (PDataRecord '[]))
  -- TODO(?) Close redeemer for pool operator / creator (fixed pkh)
  -- Close (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType MixerRedeemer where
  type DPTStrat _ = PlutusTypeData
