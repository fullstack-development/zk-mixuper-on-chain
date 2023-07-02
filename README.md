# zk-mixuper-on-chain

This project is inspired/based on previous open source work: [tornado](https://github.com/tornadocash/tornado-core), [pairing](https://github.com/sdiehl/pairing/tree/master), [cardmix](https://github.com/cardmix/Cardano-Mixer-Lib/tree/main).

## Building

```sh
nix build .#zk-mixuper-on-chain:exe:dump-script
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

## On-chain limitations

Due to execution units limits (CPU & memory) this code could not be run on-chain. Plutarch.ZK.Example runs in `ExBudget {exBudgetCPU = ExCPU 1435628658496, exBudgetMemory = ExMemory 2990514858}`.
