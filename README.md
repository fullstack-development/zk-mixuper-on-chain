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
cabal run dump-script -- -s d1ce83174feeb6ae11d95fd47cac403642cb616b244dbb32a2ca0bda -t "mixer thread" -n 100000000
```

Example if exe is build:

```sh
result/bin/dump-script -s d1ce83174feeb6ae11d95fd47cac403642cb616b244dbb32a2ca0bda -t "mixer thread" -n 100000000
```
