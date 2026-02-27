# 6502 Haskell Embedded DSL

A Haskell EDSL for writing 6502 assembly, with first-class support for Commodore 64 targets. Programs are assembled at Haskell runtime and can be exported as `.prg` files or complete `.d64` disk images.

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
cabal run 6502edsl   # Run the executable (writes hello.d64)
cabal test           # Run the test suite
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

## Quick Start

The executable in `app/Main.hs` assembles a C64 program that fills the screen with light blue blocks on a black background and writes it as a `.d64` disk image:

```haskell
import Asm.Monad (TargetConfig(..), assemble, label)
import Asm.Mos6502
import Asm.Mos6502.Control (loop_)
import Target.C64 (c64TargetConfig, defaultC64Subsystems)
import Target.C64.Mem
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

main :: IO ()
main = do
    let cfg = c64TargetConfig 0xC000 defaultC64Subsystems
        (_, bytes) = assemble cfg program
        prg = toPRG (origin cfg) bytes
        d64 = toD64 "HELLO" prg
    BS.writeFile "hello.d64" (BS.pack d64)

program :: ASM ()
program = do
    sei; cld; ldx # 0xFF; txs
    lda # colorBlack
    sta vicBorderColor
    sta vicBackgroundColor0
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
    loop_ nop
```

Run `cabal run 6502edsl` to produce `hello.d64`, which can be loaded in VICE or transferred to real hardware.

## DSL Reference

### Instructions and Addressing Modes

All 56 official 6502 opcodes are supported. Addressing modes are selected by operand type:

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
| `LDA $FB`          | `lda playerX`             | `lda (ZP 0xFB)`              | Zero page (var)    |
| `LDA $FB`          | `lda (0xFB :: Word8)`     | `lda (ZP 0xFB)`              | Zero page (raw)    |
| `LDA $0400`        | `lda (0x0400 :: Word16)`  | `lda (Abs 0x0400)`           | Absolute           |
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

Forward references require `{-# LANGUAGE RecursiveDo #-}` and `mdo` instead of `do`. This works because instruction sizes are determined eagerly; only operand values are lazy:

```haskell
mdo
    lda (0x10 :: Word8)
    beq skip
    lda # 0xFF
    skip <- label
    rts
```

Emits: `A5 10 F0 02 A9 FF 60`

### Zero-Page Variables

The assembler manages zero-page allocation. Available addresses depend on which C64 subsystems are in use:

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

Variable types:
- `allocVar8` — single zero-page byte (`Var8`)
- `allocVar16` — two contiguous bytes (`Var16`, with `lo16`/`hi16` accessors)
- `allocPtr` — two-byte pointer (`Ptr`, supports `!` indirect syntax)

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

`Asm.Mos6502.Ops16` provides 16-bit arithmetic on `Var16` zero-page pairs:

```haskell
assemble cfg $ do
    v <- allocVar16
    load16 v 0x1234            -- v = $1234
    inc16 v                    -- v++
```

Emits: `A9 34 85 02 A9 12 85 03 E6 02 D0 02 E6 03`

Available operations:
- `load16 v imm` — load immediate constant
- `inc16 v` / `dec16 v` — increment / decrement
- `add16 dst a b` / `sub16 dst a b` — addition / subtraction
- `mov16 dst src` — copy
- `cmp16 a b` — unsigned compare (sets carry for `a >= b`)

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
    (_, bytes, labels) = assembleWithLabels cfg $ do
        annotate "init" $ do
            sei; cld
        annotate "mainLoop" $ do
            loop_ nop
```

**VICE symbol file export** — generate monitor symbol files from collected labels:

```haskell
let symbols = exportViceLabels labels
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
