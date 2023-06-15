{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (throw)
import Data.ByteString.Builder (byteStringHex, hPutBuilder)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Ext.Plutarch.Extra.Run (evalT)
import Mixer.Datum
import Mixer.Datum.Plutus (MixerConfig (..))
import Mixer.Script (mkValidator, validatorLogic)
import Options (MixerOpts (..), mixerOpts)
import Options.Applicative (execParser)
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing), plet, (#))
import Plutarch.Builtin (PAsData, PData)
import Plutarch.Lift (pconstant)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V1.Bytes (LedgerBytes (..), fromHex)
import PlutusLedgerApi.V1.Value (CurrencySymbol (CurrencySymbol), TokenName, assetClass)
import PlutusLedgerApi.V2 (toData)
import Ply.Plutarch (writeTypedScript)
import System.IO (IOMode (WriteMode), withFile)

main :: IO ()
main = do
  MixerOpts {..} <- execParser mixerOpts
  ledgerCurrSymbol :: CurrencySymbol <- either throw (pure . CurrencySymbol . getLedgerBytes) $ fromHex currencySymbol
  let config =
        toData
          MixerConfig
            { protocolCurrency = ledgerCurrSymbol
            , depositTreeTokenName = fromString depositTreeTokenName
            , vaultTokenName = fromString vaultTokenName
            , nullifierStoreTokenName = fromString nullifierStoreTokenName
            , poolNominal = poolNominal
            }
  let (script, budget, log) = either (error . T.unpack) id $ evalT (mkValidator config)
  putStrLn $ "Script budget: " <> show budget
  putStrLn $ "Compilation log: " <> show log
  writeTypedScript
    (Config {tracingMode = DoTracing})
    "Mixer script with tracing"
    scriptPath
    (mkValidator config)
  pure ()
