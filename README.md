# 6502 Haskell embedded DSL

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
