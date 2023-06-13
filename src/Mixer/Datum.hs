module Mixer.Datum where

import Plutarch.Api.V2 (PCurrencySymbol, PMaybeData, PTokenName, PTuple)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

-- | A type for representing hash digests.
type PHash = PByteString

type PLovelace = PInteger

-- | Just merkle tree root for now
type PPublicInput = PInteger

newtype PWithdrawRedeemer (s :: S)
  = PWithdraw (Term s (PDataRecord '["publicInput" := PPublicInput]))
  -- TODO(?) Close redeemer for pool operator / creator (fixed pkh)
  -- Close (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PWithdrawRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PWithdrawRedeemer
instance PTryFrom PData (PAsData PWithdrawRedeemer)

newtype PWithdrawConfig (s :: S)
  = PWithdrawConfig (Term s (PDataRecord '["protocolCurrency" := PCurrencySymbol, "depositTreeTokenName" := PTokenName, "vaultTokenName" := PTokenName, "nullifierStoreTokenName" := PTokenName, "poolNominal" := PLovelace]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PWithdrawConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PWithdrawConfig
instance PTryFrom PData (PAsData PWithdrawConfig)

newtype PWithdrawDatum (s :: S)
  = PWithdrawDatum (Term s (PDataRecord '["nullifierHashes" := PBuiltinList (PAsData PInteger)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData, PEq)

instance DerivePlutusType PWithdrawDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PWithdrawDatum
instance PTryFrom PData (PAsData PWithdrawDatum)

newtype PDepositDatum (s :: S)
  = PDepositDatum (Term s (PDataRecord '["merkleTreeState" := PData, "merkleTreeRoot" := PMaybeData (PAsData PInteger)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PDepositDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PDepositDatum
instance PTryFrom PData (PAsData PDepositDatum)

data PMixerDatum (s :: S)
  = PDepositTree (Term s (PDataRecord '["depositTree" := PDepositDatum]))
  | PVault (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMixerDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMixerDatum
instance PTryFrom PData (PAsData PMixerDatum)
