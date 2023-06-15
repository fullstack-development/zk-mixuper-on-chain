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
cabal run dump-script -- -s 75a07ecddfcd14b0b5ac5b3ca3d03ee8337145166bc522a5ec1529c0
```

Example if exe is build:

```sh
result/bin/dump-script --help
result/bin/dump-script -s 75a07ecddfcd14b0b5ac5b3ca3d03ee8337145166bc522a5ec1529c0
```
