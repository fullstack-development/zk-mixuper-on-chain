{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (throw)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Ext.Plutarch.Extra.Run (evalWithArgsT)
import Mixer.Datum.Plutus (MixerConfig (..))
import Mixer.Script (validatorLogic)
import Options (MixerOpts (..), mixerOpts)
import Options.Applicative (execParser)
import PlutusLedgerApi.V1.Bytes (LedgerBytes (..), fromHex)
import PlutusLedgerApi.V1.Value (CurrencySymbol (CurrencySymbol), TokenName, assetClass)
import PlutusLedgerApi.V2 (toData)
import Plutarch.Script (serialiseScript)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Builder (byteStringHex, hPutBuilder)
import System.IO (IOMode(WriteMode), withFile)

main :: IO ()
main = do
  MixerOpts {..} <- execParser mixerOpts
  let ledgerTokenName :: TokenName = fromString tokenName
  ledgerCurrSymbol :: CurrencySymbol <- either throw (pure . CurrencySymbol . getLedgerBytes) $ fromHex currencySymbol
  let config =
        toData
          MixerConfig
            { protocolToken = assetClass ledgerCurrSymbol ledgerTokenName
            , poolNominal = poolNominal
            }
  let (script, budget, log) = either (error . T.unpack) id $ evalWithArgsT validatorLogic [config]
  putStrLn $ "Script budget: " <> show budget
  putStrLn $ "Compilation log: " <> show log
  writeShortByteStringToFile scriptPath $ serialiseScript script

writeShortByteStringToFile :: FilePath -> ShortByteString -> IO ()
writeShortByteStringToFile filePath shortByteString =
  withFile filePath WriteMode $ \handle ->
    hPutBuilder handle (byteStringHex (SBS.fromShort shortByteString))
