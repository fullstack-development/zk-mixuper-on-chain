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
cabal run dump-script -- -s bbd65a4af3dd5bb07b11cfb66418cdffc6bd26817559e0c5a80f405d
```

Example if exe is build:

```sh
result/bin/dump-script --help
result/bin/dump-script -s bbd65a4af3dd5bb07b11cfb66418cdffc6bd26817559e0c5a80f405d
```
