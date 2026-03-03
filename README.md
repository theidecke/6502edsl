# 6502 Haskell Embedded DSL

A Haskell EDSL for writing 6502 assembly, with first-class support for Commodore 64 targets. Programs are assembled at Haskell runtime and can be exported as `.prg` files, complete `.d64` disk images, or ACME cross-assembler `.asm` source files. An ACME frontend can also parse `.asm` files back into the pipeline.

The project also includes a cycle-accurate NMOS 6502 emulator with a lazy trace API, enabling time-travel debugging and differential memory analysis — all without leaving Haskell.

The ISA module (`ISA.Mos6502`) is the single source of truth for the 6502 instruction set — encoding, decoding, sizes, and cycle costs — shared between the assembler and the emulator. The assembly API uses a tagless final style (`MonadASM` typeclass) so that the same program can target both the byte assembler and the ACME text exporter.

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
cabal build              # Build everything
cabal run main           # Run the executable (writes hello.d64 + hello.vs)
cabal run color-washer   # Run the color-wash demo (writes color-washer.d64)
cabal run trace-explorer # Run the emulator trace debugging showcase
cabal test               # Run the test suite (~370 properties + Dormann)
cabal bench mem-bench    # Run memory backend benchmarks (Trie vs IntMap)
```

### REPL

Load just the library (exposes `Target.C64` etc.):

```bash
cabal repl 6502edsl
```

Load the executable (gives you `main` plus all library modules):

```bash
cabal repl exe:main
```

## Quick Start

The executable in `app/Main.hs` assembles a C64 program that fills the screen with light blue blocks on a black background and writes it as a `.d64` disk image plus a VICE symbol file:

```haskell
import Asm.Monad (ASM, TargetConfig(..), assembleWithLabels, label)
import Asm.Mos6502
import Asm.Mos6502.Control (loop_)
import Asm.Mos6502.Debug (annotate)
import Target.C64 (c64TargetConfig, defaultC64Subsystems)
import Backend.C64.D64 (toD64)
import Backend.C64.PRG (toPRG)
import Backend.C64.ViceLabels (exportViceLabels)
import Target.C64.Mem

main :: IO ()
main = do
    let cfg = c64TargetConfig 0xC000 defaultC64Subsystems
        (_, bytes, annotations, _labels) = assembleWithLabels cfg program
        prg = toPRG (origin cfg) bytes
        d64 = toD64 "HELLO" prg
    BS.writeFile "hello.d64" (BS.pack d64)
    writeFile "hello.vs" (exportViceLabels annotations)

program :: ASM ()
program = do
    annotate "init" $ do
        sei; cld; ldx # 0xFF; txs
    annotate "setColors" $ do
        lda # colorBlack
        sta vicBorderColor
        sta vicBackgroundColor0
    annotate "fillLoop" $ do
        ldx # 0x00
        fillLoop <- label
        lda # 0xA0
        sta (screenRAM, X)
        sta (screenRAM + 0x0100, X)
        sta (screenRAM + 0x0200, X)
        sta (screenRAM + 0x0300, X)
        lda # colorLightBlue
        sta (colorRAM, X)
        sta (colorRAM + 0x0100, X)
        sta (colorRAM + 0x0200, X)
        sta (colorRAM + 0x0300, X)
        inx
        bne fillLoop
    annotate "spin" $ do
        loop_ nop
```

Run `cabal run main` to produce `hello.d64`, which can be loaded in VICE or transferred to real hardware.

## DSL Reference

### Instructions and Addressing Modes

All 56 official 6502 mnemonics (151 opcode/addressing-mode combinations) are supported. Addressing modes are selected by operand type:

```haskell
lda # 0x42                    -- Immediate:       LDA #$42
lda (0x80 :: Word8)           -- Zero page:       LDA $80
lda (0x80 :: Word8, X)        -- Zero page,X:     LDA $80,X
lda (0x1234 :: Word16)        -- Absolute:        LDA $1234
lda (0x1234 :: Word16, X)     -- Absolute,X:      LDA $1234,X
lda (0x1234 :: Word16, Y)     -- Absolute,Y:      LDA $1234,Y
lda ((0x80 :: Word8) ! X)     -- Indexed indirect: LDA ($80,X)
lda ((0x80 :: Word8) ! Y)     -- Indirect indexed: LDA ($80),Y
```

Implied and accumulator modes:

```haskell
nop                           -- Implied:      NOP
sei; cld; clc; sec            -- Flag ops
dex; dey; inx; iny            -- Register ops
pha; pla                      -- Stack ops
asl A; lsr A; rol A; ror A    -- Accumulator mode (A singleton)
```

Emits: `A9 42 A5 80 B5 80 AD 34 12 BD 34 12 B9 34 12 A1 80 B1 80`

### Addressing Mode Syntax Reference

| 6502 syntax        | EDSL syntax               | Explicit constructor          | Addressing mode    |
|--------------------|---------------------------|-------------------------------|--------------------|
| `LDA #$40`         | `lda # 0x40`              | `lda (Imm 0x40)`             | Immediate          |
| `LDA $FB`          | `lda playerX`             | `lda (ZP 0xFB)`              | Zero page (Var8)   |
| `LDA $FB`          | `lda (0xFB :: Word8)`     | `lda (ZP 0xFB)`              | Zero page (raw)    |
| `LDA $0400`        | `lda flag`                | `lda (Abs 0x0400)`           | Absolute (Mem8)    |
| `LDA $0400`        | `lda (0x0400 :: Word16)`  | `lda (Abs 0x0400)`           | Absolute (raw)     |
| `LDA $FB,X`        | `lda (0xFB, X)`           | `lda (ZPX 0xFB)`             | Zero page,X        |
| `LDA $FB,Y`        | `lda (0xFB, Y)`           | `lda (ZPY 0xFB)`             | Zero page,Y        |
| `LDA $0400,X`      | `lda (0x0400, X)`         | `lda (AbsX 0x0400)`          | Absolute,X         |
| `LDA $0400,Y`      | `lda (0x0400, Y)`         | `lda (AbsY 0x0400)`          | Absolute,Y         |
| `LDA ($FB,X)`      | `lda (0xFB ! X)`          | `lda (IndX 0xFB)`            | Indirect,X         |
| `LDA ($FB),Y`      | `lda (0xFB ! Y)`          | `lda (IndY 0xFB)`            | Indirect,Y         |
| `LDA ($FB),Y`      | `lda (src ! Y)`           | `lda (IndY 0xFB)`            | Indirect,Y (ptr)   |
| `ASL A`            | `asl A`                   | `asl_a`                       | Accumulator        |
| `JMP ($FFFC)`      | `jmpInd 0xFFFC`           | `jmp_ind 0xFFFC`             | Indirect (JMP)     |

The comma/bang distinction mirrors the 6502: comma means indexed (`addr,X`), bang means indirect (`(addr,X)` or `(addr),Y`). Zero page vs absolute is inferred from `Word8` vs `Word16`.

### Labels and Backward Branches

Labels are the current program counter, returned by `label`. Backward branches work naturally in `do` notation:

```haskell
loop <- label
dex
bne loop
```

Emits: `CA D0 FD`

### Forward References with `mdo`

Forward references require `{-# LANGUAGE RecursiveDo #-}` and `mdo` instead of `do`. This works because `instrSize` is determined eagerly from the `AddressingMode` constructor (never forcing operand thunks); only operand values are lazy:

```haskell
mdo
    lda (0x10 :: Word8)
    beq skip
    lda # 0xFF
    skip <- label
    rts
```

Emits: `A5 10 F0 02 A9 FF 60`

### Typed Memory Locations

The DSL provides symmetric typed wrappers for zero-page and absolute memory locations:

|          | ZP (`Word8` base)     | Absolute (`Word16` base)  |
|----------|-----------------------|---------------------------|
| 1-byte   | `Var8`                | `Mem8`                    |
| 2-byte   | `Var16`               | `Mem16`                   |
| Pointer  | `Ptr`                 | `MemPtr`                  |

**Zero-page variables** are allocated from a managed pool (available addresses depend on which C64 subsystems are in use):

```haskell
let cfg = c64TargetConfig 0x0800 defaultC64Subsystems { useBasic = False }
assemble cfg $ do
    ptr <- allocPtr             -- Allocate 2 ZP bytes as a pointer
    lda # 0x00
    sta ptr                     -- Store to first byte
    sta (ptr + 1)               -- Pointer arithmetic
    lda (ptr ! Y)               -- Indirect indexed: LDA (ptr),Y
    rts
```

Emits: `A9 00 85 02 85 03 B1 02 60`

Allocators:
- `allocVar8` — single zero-page byte (`Var8`)
- `allocVar16` — two contiguous bytes (`Var16`)
- `allocPtr` — two-byte pointer (`Ptr`, supports `!` indirect syntax)

**Absolute memory locations** are constructed directly:

```haskell
let counter = Mem16 0xC600      -- 16-bit value at $C600/$C601
    flag    = Mem8  0xC800      -- single byte at $C800
```

The `Loc16` typeclass unifies 2-byte locations (`Var16`, `Ptr`, `Mem16`, `MemPtr`) with `lo16`/`hi16` accessors that return the appropriate byte type (`Var8` for ZP, `Mem8` for absolute). All Ops16 functions work with any `Loc16` instance.

### Data Embedding

`Target.C64.Data` provides helpers for embedding data in the output stream:

```haskell
byte [0x01, 0x02, 0x03]       -- Raw bytes
word [0x1234, 0x5678]          -- Little-endian 16-bit words
petscii "HI"                   -- PETSCII string (uppercase mode)
pstring "HELLO"                -- Null-terminated PETSCII string
```

Emits: `01 02 03 34 12 78 56 48 49`

`charToPetscii` is exported for direct use. Lowercase `a`–`z` maps to PETSCII `$C1`–`$DA`; other printable ASCII passes through.

### Memory Alignment

`Asm.Mos6502.Memory` provides alignment primitives:

```haskell
align 256                      -- Pad with $00 until PC is page-aligned
alignPage                      -- Shorthand for align 256
```

Useful for lookup tables that must not cross page boundaries.

### Control Flow Combinators

`Asm.Mos6502.Control` provides structured control flow that handles branch wiring automatically:

**Conditional:**

```haskell
lda # 0x05
cmp # 0x05
if_eq
    (lda # 0x01)               -- then: Z=1 (equal)
    (lda # 0x02)               -- else
rts
```

Emits: `A9 05 C9 05 D0 05 A9 01 4C 0D C0 A9 02 60` (at origin `$C000`)

Available variants: `if_eq`, `if_ne`, `if_cs`, `if_cc`, `if_pl`, `if_mi`, and the generalized `if_` which takes a branch function directly.

**One-armed when (no else branch):**

```haskell
lda # 0x05
cmp # 0x05
when_eq (lda # 0x01)              -- body runs only when Z=1
rts
```

Emits: `A9 05 C9 05 D0 02 A9 01 60` — no JMP, tighter than `if_eq` with an empty else.

Available variants: `when_eq`, `when_ne`, `when_cs`, `when_cc`, `when_pl`, `when_mi`, and the generalized `when_`.

**Counted loops:**

```haskell
for_x 0x0A $ do               -- X = 10, 9, ..., 1
    nop
```

Emits: `A2 0A EA CA D0 FC`

Also `for_y` for the Y register.

**While loop:**

```haskell
while_ beq                     -- Exit when Z=1
    (lda (0x80 :: Word8))      -- Condition
    (inc (0x80 :: Word8))      -- Body
```

**Infinite loop:**

```haskell
loop_ $ do
    inc (0x80 :: Word8)
```

Emits: `E6 80 4C 00 C0` (at origin `$C000`)

### 16-bit Operations

`Asm.Mos6502.Ops16` provides 16-bit arithmetic on any `Loc16` instance (ZP or absolute):

```haskell
assemble cfg $ do
    v <- allocVar16
    load16 v 0x1234            -- v = $1234
    inc16 v                    -- v++

-- Also works with absolute memory:
let counter = Mem16 0xC600
lshift16 counter               -- counter <<= 1
```

Emits (ZP example): `A9 34 85 02 A9 12 85 03 E6 02 D0 02 E6 03`

Available operations:
- `load16 v imm` — load immediate constant
- `inc16 v` / `dec16 v` — increment / decrement
- `add16 dst a b` / `sub16 dst a b` — addition / subtraction
- `mov16 dst src` — copy
- `cmp16 a b` — unsigned compare (sets carry for `a >= b`)
- `lshift16 v` / `rshift16 v` — 16-bit left / right shift

### Debug and Assertions

`Asm.Mos6502.Debug` provides compile-time assertions and code annotation for debugging.

**Size assertions** — verify a block fits within a byte budget:

```haskell
fitsIn 256 $ do
    -- lookup table that must fit in one page
    byte [0..255]
```

If the block exceeds the limit, assembly fails with an error.

**Page-boundary assertions** — verify the PC is on the same page as an address:

```haskell
tableStart <- label
byte sineTable
samePage tableStart    -- error if table crossed a page boundary
```

`samePage` is in `Asm.Mos6502.Memory`.

**Code annotation** — name regions for debug symbol export:

```haskell
let cfg = c64TargetConfig 0xC000 defaultC64Subsystems
    (_, bytes, annotations, _labels) = assembleWithLabels cfg $ do
        annotate "init" $ do
            sei; cld
        annotate "mainLoop" $ do
            loop_ nop
```

**VICE symbol file export** — generate monitor symbol files from collected annotations:

```haskell
let symbols = exportViceLabels annotations
writeFile "program.vs" symbols
-- File contents:
-- al C:C000 .init
-- al C:C002 .mainLoop
```

Load in VICE with `mon_commands "program.vs"` or via the monitor command `ll "program.vs"`.

### C64 Memory Map

`Target.C64.Mem` exports named constants for the full C64 memory map:

- **VIC-II** (`$D000`–`$D02E`): `vicBorderColor`, `vicBackgroundColor0`, `vicSpriteEnable`, `vicControlY`, etc.
- **SID** (`$D400`–`$D41C`): `sidVoice1FreqLo`, `sidVolumeFilterMode`, etc.
- **CIA 1/2** (`$DC00`/`$DD00`): `cia1DataPortA`, `cia2DataPortA`, etc.
- **KERNAL** (`$FF81`–`$FFF3`): `kernChrout`, `kernGetin`, `kernScnkey`, etc.
- **System**: `screenRAM`, `colorRAM`, `colorBlack`–`colorLightGrey`, etc.

### Output Formats

**PRG file** (2-byte load address header + raw bytes):

```haskell
let prg = toPRG 0xC000 bytes
```

**D64 disk image** (174,848 bytes, directly usable in VICE):

```haskell
let d64 = toD64 "DISKNAME" prg
BS.writeFile "output.d64" (BS.pack d64)
```

**ACME assembly text** — export an EDSL program as ACME cross-assembler source:

```haskell
import Backend.ACME (exportAcmeWith)

writeFile "output.asm" (exportAcmeWith cfg program)
```

This requires `program` to have a polymorphic signature (`MonadASM m => ...` or `(MonadASM m, MonadZPAlloc m) => ...`) so it can be instantiated at both the byte assembler (`ASM`) and the text exporter (`AcmeASM`).

### ACME Import

`Frontend.ACME` provides a parser and assembler for ACME `.asm` files:

```haskell
import Frontend.ACME.Parser (parseAcme)
import Frontend.ACME.Assemble (assembleAcme)

let Right ast = parseAcme sourceText
    bytes     = assembleAcme ast
```

Together with `Backend.ACME`, this enables a full roundtrip: EDSL program → ACME text → parse → assemble → identical bytes.

### Tagless Final Assembly

The assembly API uses a tagless final style via the `MonadASM` typeclass. Programs written with polymorphic signatures work with both backends:

```haskell
-- Works with both ASM (byte output) and AcmeASM (text output)
myRoutine :: MonadASM m => m ()
myRoutine = do
    lda # 0x42
    sta (0x0400 :: Word16)

-- Programs using ZP allocation need both constraints
myProgram :: (MonadASM m, MonadZPAlloc m) => m ()
myProgram = do
    ptr <- allocPtr
    lda # 0x00
    sta ptr
```

## Emulator

The project includes a cycle-accurate NMOS 6502 emulator (`Emu.*` modules) that shares the ISA module with the assembler. It implements all 151 official instructions including decimal mode (BCD) arithmetic with correct NMOS flag behavior, the JMP indirect page boundary bug, and proper BRK/RTI semantics.

The emulator is validated against the [Klaus Dormann 6502 functional test suite](https://github.com/Klaus2m5/6502_65C02_functional_tests), which exhaustively tests all instructions, addressing modes, and flag behavior including decimal mode. The test runs ~96 million cycles to completion.

### CPU State and Lenses

`Emu.CPU` provides the CPU state and hand-rolled van Laarhoven lenses (compatible with `microlens`/`lens`):

```haskell
import Emu.CPU

let s = initCPU               -- A=0, X=0, Y=0, SP=0xFD, P=0x24, PC=0, cycles=0
view regA s                    -- 0
set regPC 0x0800 s             -- set program counter
over regX (+1) s               -- increment X
view flagC s                   -- read carry flag (Bool)
set flagZ True s               -- set zero flag
```

Register lenses: `regA`, `regX`, `regY`, `regSP`, `regP`, `regPC`, `mem`, `cycles`

Flag bit lenses: `flagC`, `flagZ`, `flagI`, `flagD`, `flagB`, `flagV`, `flagN`

Composite lens: `memAt addr` — lens into a single memory byte

### Execution

`Emu.Step` provides fetch-decode-execute:

```haskell
import Emu.Step (step)

let s1 = step s0               -- execute one instruction at PC
```

`step` reads the opcode at PC, decodes it via the O(1) lookup table, and dispatches to `execute`. Cycle counts are accumulated, including page-crossing penalties.

### Lazy Traces

`Emu.Trace` provides a lazy infinite list of CPU states and observation combinators:

```haskell
import Emu.Trace

-- Load a program and run it
let s0  = loadProgram 0x0800 bytes initCPU
    tr  = trace s0             -- infinite lazy [CPUState]
    s10 = runN 10 s0           -- skip ahead 10 steps
    end = runUntil (\s -> view regPC s == haltAddr) s0
```

**Observation combinators** — project values from traces without forcing unnecessary states:

```haskell
-- Track a register over time
watchReg regA (take 20 tr)     -- [Word8]: A register at each step

-- Watch a memory address evolve
watchMem 0x0400 (take 100 tr)  -- [Word8]: byte at $0400 at each step

-- Read a 16-bit ZP variable
watch16 0x02 someState         -- Word16: little-endian from $02/$03

-- Memory diffs between consecutive states
deltas (take 50 tr)            -- [[(Word16, Word8, Word8)]]: (addr, old, new)

-- PC coverage histogram (address → execution count)
pcCoverage (take 1000 tr)      -- Map Word16 Int: hot-loop detection
```

**Time-travel debugging** — the laziness of traces enables patterns like binary search over execution history:

```haskell
-- Find the exact step that writes to $0400
let tr = trace s0
    check n = readByte 0x0400 (cpuMem (tr !! n)) /= 0
    -- Binary search: bisect [0..1000] check
```

### Memory Backend

Emulator RAM is backed by a persistent lazy nibble trie (`Emu.Mem.Trie`): a 4-level trie keyed on address nibbles with 16-way branching via `SmallArray`. This provides:

- O(1) read/write (4 hops)
- ~512 bytes allocation per write (copy-on-write)
- Structural sharing across trace states
- Pointer equality optimization in `diffMem` — skips shared subtrees, making diffs of nearly-identical states nearly free
- Lazy leaf values — preserves thunks from MonadFix assembly through emulation

The original `IntMap.Lazy` implementation is retained as `Emu.Mem.IntMap` for benchmarking. Run `cabal bench mem-bench` to compare.

### Emulator Example

The `trace-explorer` example (`examples/TraceExplorer.hs`) demonstrates the lazy trace API with seven interactive demos:

1. **Register evolution** — track a countdown loop's X register
2. **Memory observation** — watch writes to an address over time
3. **16-bit variable tracking** — observe carry-propagating increments via `watch16`
4. **Time-travel debugging** — find the exact instruction that modifies a given address
5. **Cross-program RAM diff** — compare final states of two programs computing the same result differently
6. **Trace bisection** — binary search over a trace to find when a screen write occurs
7. **PC coverage** — execution profile histogram via `pcCoverage`, showing hot/warm/cold paths in a loop with conditional branches

```bash
cabal run trace-explorer
```

## Project Structure

```
src/
  ISA/Mos6502.hs              -- 6502 ISA: types, encode, decode, instrSize, baseCycles, canPageCross
  Asm/
    Monad.hs                   -- MonadASM/MonadZPAlloc typeclasses, ASM concrete monad,
                               --   MonadFix for forward refs, ZP allocator
    Mos6502.hs                 -- Operand typeclass, addressing mode sugar, typed memory
                               --   locations (Var8/Var16/Ptr, Mem8/Mem16/MemPtr), Loc16 typeclass
    Mos6502/
      Control.hs               -- Structured control flow (if_eq, when_eq, while_, for_x, loop_)
      Ops16.hs                 -- 16-bit arithmetic via Loc16 (add16, inc16, lshift16, etc.)
      Memory.hs                -- Alignment and page assertions
      Debug.hs                 -- Size assertions, named labels
  Backend/
    ACME.hs                    -- ACME .asm text export (AcmeASM monad, exportAcme, exportAcmeWith)
    C64/
      PRG.hs                   -- .prg file format (2-byte header + bytes)
      D64.hs                   -- .d64 disk image generation
      ViceLabels.hs            -- VICE monitor symbol file export
  Frontend/
    ACME/
      Syntax.hs                -- ACME AST types
      Parser.hs                -- ACME .asm parser (parseAcme)
      Assemble.hs              -- ACME AST → bytes assembler (assembleAcme)
  Emu/
    Mem.hs                     -- Re-export of Emu.Mem.Trie
    Mem/
      Trie.hs                 -- Persistent lazy nibble trie (primary backend)
      IntMap.hs               -- IntMap implementation (retained for benchmarking)
    CPU.hs                     -- CPU state, hand-rolled van Laarhoven lenses, flag lenses
    Step.hs                    -- Instruction execution (all 151 opcodes, cycle counting)
    Trace.hs                   -- Lazy trace, runUntil, runN, loadProgram, observation combinators
  Target/
    C64.hs                     -- C64 target config, subsystem-aware ZP free list
    C64/
      Data.hs                  -- Data embedding (byte, word, petscii)
      RomLabels.hs             -- Named labels for BASIC/KERNAL ROM routines
      Mem.hs                   -- Full C64 memory map (VIC, SID, CIA, KERNAL)
app/Main.hs                    -- Example: colored screen fill
examples/
  ColorWasher.hs               -- Example: animated color wash with SID audio
  TraceExplorer.hs             -- Example: emulator trace debugging showcase
bench/Main.hs                  -- Memory backend benchmarks (Trie vs IntMap)
test/
  Main.hs                      -- Test runner (~370 properties)
  data/
    6502_functional_test.bin   -- Klaus Dormann's 6502 functional test (65536 bytes)
  Test/
    Helpers.hs                 -- Shared test infrastructure
    ISA.hs                     -- ISA encode/decode/roundtrip tests
    Instructions.hs            -- EDSL instruction + sugar tests
    Monad.hs                   -- Monad, branches, ZP allocation tests
    Control.hs                 -- Control flow + 16-bit ops tests
    Memory.hs                  -- Alignment, assertions, labels tests
    Target.hs                  -- PRG, D64, data embedding, VICE export tests
    ACME.hs                    -- ACME export + roundtrip tests
    Label.hs                   -- Label and namedLabel tests
    Emu/
      Mem.hs                   -- Mem properties: read/write identity, isolation
      CPU.hs                   -- Lens laws, flag positions, updateNZ, memAt
      Step.hs                  -- All instruction categories (load/store, arithmetic, etc.)
      Trace.hs                 -- Trace/runN/loadProgram/combinator properties
      Integration.hs           -- Assembler-to-emulator bridge tests
      Dormann.hs               -- Klaus Dormann functional test (~96M cycles)
      Laziness.hs              -- End-to-end laziness: undefined bytes survive load/step/trace
```

**Dependencies**: `base`, `array`, `containers`, `primitive` (library); adds `bytestring` (executables), `QuickCheck` + `bytestring` (tests), `tasty-bench` + `deepseq` (benchmarks).

## Acknowledgments

The emulator test suite uses the [6502 functional test](https://github.com/Klaus2m5/6502_65C02_functional_tests) by Klaus Dormann, which provides exhaustive validation of NMOS 6502 instruction behavior.
