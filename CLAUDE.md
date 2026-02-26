# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Haskell embedded DSL for writing 6502 assembly code. Early-stage project (scaffolding only). Part of a broader Commodore 64 / retro computing research effort.

## Build Commands

```bash
cabal build            # Build the project
cabal run 6502edsl     # Run the executable
cabal repl             # Interactive GHC REPL
cabal clean            # Clean build artifacts
```

No test suite is configured yet. When one is added, it will likely use `cabal test`.

## Architecture

- **Build system**: Cabal (spec 3.0), GHC2024 language edition, GHC 9.10.1
- **Source layout**: Single executable, source in `app/`
- **Dependencies**: Only `base` so far
- **Compiler warnings**: `-Wall` enabled via the `warnings` common stanza in the cabal file
