{-# LANGUAGE DerivingStrategies #-}

module Options where

import Data.ByteString (ByteString)
import Options.Applicative

data MixerOpts = MixerOpts
  { currencySymbol :: ByteString
  , depositTreeTokenName :: String
  , vaultTokenName :: String
  , nullifierStoreTokenName :: String
  , poolNominal :: Integer
  , scriptPath :: FilePath
  }
  deriving stock (Show, Eq)

mixerParser :: Parser MixerOpts
mixerParser =
  MixerOpts
    <$> strOption
      ( long "currency-symbol"
          <> short 's'
          <> metavar "CURRENCY_SYMBOL"
          <> help "Currency symbol of protocol thread token"
      )
    <*> strOption
      ( long "tree-token-name"
          <> value "Deposit Tree Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of deposit tree protocol thread token"
      )
    <*> strOption
      ( long "vault-token-name"
          <> value "Vault Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of vault protocol thread token"
      )
    <*> strOption
      ( long "store-token-name"
          <> value "Nullifier Store Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of nullifier store protocol thread token"
      )
    <*> option
      auto
      ( long "pool-nominal"
          <> short 'n'
          <> value 100_000_000
          <> help "Nominal value of the mixer pool in lovelace"
          <> metavar "INT"
      )
    <*> strOption
      ( long "path"
          <> help "Path where the script is written"
          <> showDefault
          <> value "compiled/mixerScript.plutus"
          <> metavar "SCRIPT_PATH"
      )

mixerOpts :: ParserInfo MixerOpts
mixerOpts =
  info
    (mixerParser <**> helper)
    ( fullDesc
        <> progDesc "Dump script byte code to a file"
        <> header "dump-script - write plutus script to a file"
    )
