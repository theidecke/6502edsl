# 6502 Haskell embedded DSL

## Setup

Install prerequisites (Ubuntu/Debian):

```bash
sudo apt install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 pkg-config
```

Install [ghcup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Install GHC 9.10.1 and set it as default:

```bash
ghcup install ghc 9.10.1
ghcup set ghc 9.10.1
```

ghcup also installs `cabal-install`. Verify your toolchain:

```bash
ghc --version    # 9.10.1
cabal --version
```

## Development

```bash
cabal build          # Build everything
cabal run 6502edsl   # Run the executable
```

### REPL

Load just the library (exposes `Target.C64` etc.):

```bash
cabal repl 6502edsl
```

Load the executable (gives you `main` plus all library modules):

```bash
cabal repl exe:6502edsl
```
