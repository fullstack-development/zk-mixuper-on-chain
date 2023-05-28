{-# LANGUAGE DerivingStrategies #-}

module Options where

import Options.Applicative

data MixerOpts = MixerOpts
  { currencySymbol :: String
  , tokenName :: String
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
      ( long "token-name"
          <> short 't'
          <> metavar "TOKEN_NAME"
          <> help "Name of protocol thread token"
      )
    <*> option
      auto
      ( long "pool-nominal"
          <> short 'n'
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
