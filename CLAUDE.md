# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Haskell embedded DSL for writing 6502 assembly code, targeting the Commodore 64. Programs are assembled at Haskell runtime and exported as `.prg` files or complete `.d64` disk images. Part of a broader retro computing research effort.

## Build Commands

```bash
cabal build            # Build the project (library + executable)
cabal run 6502edsl     # Run the executable (writes hello.d64 + hello.vs)
cabal test             # Run the QuickCheck test suite (~100 properties)
cabal repl             # Interactive GHC REPL (library modules)
cabal repl exe:6502edsl  # REPL with executable + library
cabal clean            # Clean build artifacts
```

## Architecture

- **Build system**: Cabal (spec 3.0), GHC2024 language edition, GHC 9.10.1
- **Version**: 0.2.0.0
- **Source layout**: Library in `src/`, executable in `app/`, tests in `test/`
- **Dependencies**: `base`, `containers` (library); adds `bytestring` (exe), `QuickCheck` (test)
- **Compiler warnings**: `-Wall` enabled; project builds with zero warnings

### Module Structure

```
src/
  ISA/Mos6502.hs              -- Opcode/AddressingMode/Instruction data types (all 56 official mnemonics)
  Asm/
    Monad.hs                   -- ASM monad (State + DList writer), MonadFix for forward refs, ZP allocator
    Mos6502.hs                 -- Instruction emission, operand typeclasses, addressing mode sugar (#, !, tuples)
    Mos6502/
      Control.hs               -- Structured control flow (if_eq, while_, for_x, loop_, etc.)
      Ops16.hs                 -- 16-bit arithmetic on Var16 (add16, inc16, cmp16, etc.)
      Memory.hs                -- Alignment (align, alignPage) and page assertions (samePage)
      Debug.hs                 -- fitsIn size assertion, annotate for named labels
  Target/
    C64.hs                     -- C64 target config, subsystem-aware ZP free list
    C64/
      PRG.hs                   -- .prg file format (2-byte header + bytes)
      D64.hs                   -- .d64 disk image generation (BAM, directory, sector chains)
      Data.hs                  -- Data embedding (byte, word, petscii, pstring)
      Debug.hs                 -- VICE monitor symbol file export
      Mem.hs                   -- Re-exports all Mem.* submodules
      Mem/
        VIC.hs                 -- VIC-II registers ($D000-$D02E)
        SID.hs                 -- SID registers ($D400-$D41C)
        CIA1.hs                -- CIA 1 registers ($DC00-$DC0F)
        CIA2.hs                -- CIA 2 registers ($DD00-$DD0F)
        Kernal.hs              -- KERNAL jump table + hardware vectors
        System.hs              -- CPU port, system vectors, screen/color RAM, color constants
app/Main.hs                    -- Example program: fills C64 screen with colored blocks
test/Main.hs                   -- ~100 QuickCheck properties covering all modules
```

### Key Design Decisions

- **Single-pass assembly via MonadFix**: Forward label references use `mdo`/`RecursiveDo`. Works because instruction sizes are determined eagerly; only operand values are lazy.
- **Addressing modes via typeclasses**: `Operand` typeclass with instances for newtypes (`Imm`, `ZP`, `Abs`, etc.), bare types (`Word8` = ZP, `Word16` = Abs), tuples (`(addr, X)`), and singletons (`A`).
- **`(#)` operator**: Immediate mode sugar (`lda # 0x42`).
- **`(!)` operator**: Indirect mode sugar via `Indirectable` typeclass (`ptr ! Y`).
- **Typed ZP variables**: `Var8`, `Var16`, `Ptr` allocated from a `Set Word8` free pool.
- **Assembly-time errors**: `error` calls for invalid addressing modes, out-of-ZP, branch range issues, `fitsIn`/`samePage` violations.
- **Output formats**: `[Word8]` lists throughout; `toPRG` adds load address header; `toD64` builds complete disk image.

## Conventions

- All opcodes use lowercase names matching 6502 mnemonics (e.g., `lda`, `sta`, `jsr`)
- `and_` has trailing underscore to avoid Prelude conflict
- Accumulator-mode shift/rotate: `asl_a` / `asl A` (both work)
- Memory map constants: `vic*`, `sid*`, `cia1*`/`cia2*`, `kernal*`, `sys*`, `color*` prefixes
- Tests use deterministic QuickCheck (seed 42, 1000 cases for parametric props)
