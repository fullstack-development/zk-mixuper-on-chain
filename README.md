# Tornado.cash on Cardano

## Building

```sh
nix build .#tornadano:exe:dump-script
```

## Development shell

You can use cabal, haskell-language-server and other dev tools inside.

```sh
nix develop
```

## Usage

Example in nix dev shell:

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s cd4ecd8b80466c7325e9d2f76fce6eb8a236667734eb1646bcfdcb51
```

Example if exe is build:

```sh
result/bin/dump-script --help
result/bin/dump-script -s cd4ecd8b80466c7325e9d2f76fce6eb8a236667734eb1646bcfdcb51
```
