# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Haskell embedded DSL for writing 6502 assembly code, targeting the Commodore 64. Programs are assembled at Haskell runtime and exported as `.prg` files or complete `.d64` disk images. Part of a broader retro computing research effort.

## Build Commands

```bash
cabal build            # Build the project (library + executable)
cabal run main         # Run the executable (writes hello.d64 + hello.vs)
cabal test             # Run the QuickCheck test suite (~190 properties + Dormann)
cabal repl             # Interactive GHC REPL (library modules)
cabal repl exe:main      # REPL with executable + library
cabal clean            # Clean build artifacts
```

## Architecture

- **Build system**: Cabal (spec 3.0), GHC2024 language edition, GHC 9.10.1
- **Version**: 0.2.0.0
- **Source layout**: Library in `src/`, executable in `app/`, tests in `test/`
- **Dependencies**: `base`, `array`, `containers`, `primitive` (library); adds `bytestring` (exe), `QuickCheck` + `bytestring` (test); `tasty-bench` + `deepseq` (benchmark)
- **Compiler warnings**: `-Wall` enabled; project builds with zero warnings

### Module Structure

```
src/
  ISA/Mos6502.hs              -- Single source of truth for the 6502 ISA:
                               --   data types (Opcode, AddressingMode, Instruction)
                               --   encode (151-arm pattern match → [Word8])
                               --   decode (O(1) Array lookup → Instruction constructor)
                               --   instrSize, baseCycles, canPageCross
  Asm/
    Monad.hs                   -- ASM monad (State + DList writer), MonadFix for forward refs, ZP allocator
    Mos6502.hs                 -- Operand typeclass (toAddrMode), addressing mode sugar (#, !, tuples),
                               --   instruction emission via ISA.encode
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
      Mem.hs                   -- Re-exports all Mem.* submodules (VIC, SID, CIA, KERNAL, System)
      Mem/
        VIC.hs                 -- VIC-II registers ($D000-$D02E)
        SID.hs                 -- SID registers ($D400-$D41C)
        CIA1.hs                -- CIA 1 registers ($DC00-$DC0F)
        CIA2.hs                -- CIA 2 registers ($DD00-$DD0F)
        Kernal.hs              -- KERNAL jump table + hardware vectors
        System.hs              -- CPU port, system vectors, screen/color RAM, color constants
  Emu/
    Mem.hs                     -- Thin re-export of Emu.Mem.Trie
    Mem/
      Trie.hs                 -- Persistent lazy nibble trie (primary): 4-level, 16-way SmallArray
      IntMap.hs               -- IntMap-backed implementation (retained for benchmarking)
    CPU.hs                     -- CPUState, hand-rolled van Laarhoven lenses, flag bit lenses,
                               --   memAt composite lens, updateNZ
    Step.hs                    -- execute (Instruction → CPUState → CPUState),
                               --   step (fetch-decode-execute), all 151 instructions,
                               --   BCD decimal mode, JMP indirect page bug, cycle counting
    Trace.hs                   -- trace (lazy infinite [CPUState]), runUntil, runN, loadProgram
app/Main.hs                    -- Example program: fills C64 screen with colored blocks
test/
  Main.hs                      -- Test runner (imports all test modules)
  data/
    6502_functional_test.bin   -- Klaus Dormann's 6502 functional test (65536 bytes)
  Test/
    Helpers.hs                 -- Shared test infrastructure (runner, TestInsn, Arbitrary instances)
    ISA.hs                     -- ISA tests: lo/hi, table integrity, encode/decode, sizes, cycles
    Instructions.hs            -- EDSL instruction functions + addressing mode sugar
    Monad.hs                   -- Monad/PC tracking, branches, ZP allocation
    Control.hs                 -- Structured control flow + 16-bit operations
    Memory.hs                  -- Alignment, samePage, fitsIn, annotate, assembleWithLabels
    Target.hs                  -- PRG, D64, data embedding, VICE label export
    Emu/
      Mem.hs                   -- Mem properties: read/write identity, isolation, persistence
      CPU.hs                   -- Lens laws, flag bit positions, updateNZ, memAt
      Step.hs                  -- All instruction categories: load/store, transfer, stack,
                               --   ADC/SBC binary+decimal, logic, compare, shift/rotate,
                               --   inc/dec, branches, jumps, flags, BIT, BRK, NOP, cycles
      Trace.hs                 -- Trace/runN/loadProgram properties
      Integration.hs           -- Assembler→emulator bridge (assemble then execute)
      Dormann.hs               -- Klaus Dormann functional test (~96M cycles to success)
      Laziness.hs              -- End-to-end laziness: undefined bytes survive load/step/trace
```

### Key Design Decisions

- **ISA as single source of truth**: `ISA.Mos6502` owns all encoding/decoding knowledge. Both the assembler and emulator depend on it, but not on each other. The `encode` function is a direct 151-arm pattern match; `decode` uses an O(1) `Array Word8` lookup table. Also exports `lo`, `hi`, `w16` used by both.
- **Single-pass assembly via MonadFix**: Forward label references use `mdo`/`RecursiveDo`. Works because instruction sizes are determined eagerly (via `instrSize` on `AddressingMode` constructors); only operand values are lazy.
- **Addressing modes via typeclasses**: `Operand` typeclass with a single method `toAddrMode :: a -> AddressingMode`. Instances for newtypes (`Imm`, `ZP`, `Abs`, etc.), bare types (`Word8` = ZP, `Word16` = Abs), tuples (`(addr, X)`), and singletons (`A`).
- **`(#)` operator**: Immediate mode sugar (`lda # 0x42`).
- **`(!)` operator**: Indirect mode sugar via `Indirectable` typeclass (`ptr ! Y`).
- **Typed ZP variables**: `Var8`, `Var16`, `Ptr` allocated from a `Set Word8` free pool.
- **Assembly-time errors**: `error` calls for invalid addressing modes, out-of-ZP, branch range issues, `fitsIn`/`samePage` violations.
- **Output formats**: `[Word8]` lists throughout; `toPRG` adds load address header; `toD64` builds complete disk image.
- **Emulator lenses**: Hand-rolled van Laarhoven `Lens'` using only `Const`/`Identity` from `base` (~50 lines). Type-compatible with `microlens`/`lens` for future migration. Flag lenses address individual bits of the P register.
- **Emulator memory**: Persistent lazy nibble trie (`Emu.Mem.Trie`, primary) — 4-level trie keyed on address nibbles, 16-way branching via `SmallArray` from `primitive`. O(1) read/write (4 hops), ~512 bytes allocation per write, structural sharing across trace states. `Leaf` is deliberately lazy in `Word8` to preserve thunks from MonadFix assembly. `Default` nodes represent uninitialized subtrees (read as 0x00). `diffMem` uses `reallyUnsafePtrEquality#` to skip shared subtrees. The original `IntMap.Lazy` implementation is retained as `Emu.Mem.IntMap` for benchmarking comparison.
- **Emulator validation**: Klaus Dormann's 6502 functional test exercises all 151 instructions including decimal mode. Success address `$3469` after ~96M cycles.

## Conventions

- All opcodes use lowercase names matching 6502 mnemonics (e.g., `lda`, `sta`, `jsr`)
- `and_` has trailing underscore to avoid Prelude conflict
- Accumulator-mode shift/rotate: `asl_a` / `asl A` (both work)
- Memory map constants: `vic*`, `sid*`, `cia1*`/`cia2*`, `kernal*`, `sys*`, `color*` prefixes
- Tests use deterministic QuickCheck (seed 42, 1000 cases for parametric props)
- Tests are split into focused modules under `test/Test/` and `test/Test/Emu/`, each exporting a `tests :: [IO Bool]` list
- The Dormann test uses `checkIO` for IO-based validation (loads `.bin` file from `test/data/`)


## General development principles

- We don't need backwards compatibility, rather strive for a clean, concise and current codebase
