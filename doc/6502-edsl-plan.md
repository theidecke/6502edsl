# Embedded 6502 Assembler DSL in Haskell — Implementation Plan

## Project Goal

Build an embedded domain-specific language (EDSL) in Haskell that lets users write 6502 assembly for the Commodore 64 using concise, type-checked Haskell syntax that closely mirrors real assembly. This assembler is the foundation layer for a larger project: building upward through typed variables, 16-bit operations, control flow combinators, an expression compiler, and eventually a full optimizing compiler for a high-level language targeting the C64. The compiled output is a `.prg` binary runnable on real hardware or in the VICE emulator.

---

## Part 1: Core ASM Monad with MonadFix Labels

### Architecture

The assembler monad tracks a byte offset and accumulates output bytes. It must support `MonadFix` so that forward label references resolve via lazy knot-tying, eliminating the need for two-pass assembly or backpatching.

### Implementation

```haskell
{-# LANGUAGE RecursiveDo #-}

newtype ASM a = ASM (Word16 -> (a, Word16, Endo [Word8]))
```

The monad is essentially a combination of Reader (base address), Writer (output bytes), and State (current offset), fused into a single function type for performance.

Required instances: `Functor`, `Applicative`, `Monad`, `MonadFix`.

### Key primitives

```haskell
emit :: [Word8] -> ASM ()
-- Appends bytes to output, advances offset by length of bytes.

here :: ASM Word16
-- Returns current offset without emitting anything. This is how labels work.

runASM :: Word16 -> ASM () -> [Word8]
-- Executes the assembly starting at a base address, returns byte output.
```

### MonadFix instance (critical)

```haskell
instance MonadFix ASM where
  mfix f = ASM $ \pos ->
    let (a, pos', out) = runASM' (f a) pos
    in  (a, pos', out)
```

This allows forward references:

```haskell
mdo beq skip
    lda # 0xFF
    sta playerX
    skip <- here
```

The value `skip` is a thunk that gets resolved when execution reaches `here`. This works because instruction *sizes* are always known eagerly (BEQ is always 2 bytes, JMP is always 3), and only the operand *values* are lazy.

### Branch instruction helpers

```haskell
branch :: Word8 -> Word16 -> ASM ()
-- Emits a 2-byte relative branch. The target address is lazy.
-- Computes: relative_offset = target - current_position - 2

beq, bne, bcc, bcs, bpl, bmi, bvs, bvc :: Word16 -> ASM ()
-- Each calls `branch` with the appropriate opcode byte.

jmp :: Word16 -> ASM ()
-- Emits 3-byte absolute jump (opcode 0x4C + little-endian address).
-- Target address is lazy.
```

### Branch range safety

After the full program is assembled, run a validation pass that checks every relative branch is within -128..+127. Do NOT try to handle branch relaxation (expanding to branch-over-jump) inside the MonadFix knot, as this changes instruction sizes and breaks the lazy resolution. Instead, either report an error or do a separate post-pass that expands out-of-range branches and reassembles.

---

## Part 2: Addressing Mode Syntax via Typeclasses

### Design principle

The addressing mode is determined by the *type* of the operand passed to an instruction function. Typeclasses dispatch to the correct opcode. The user never writes explicit addressing mode constructors like `Imm`, `ZP`, `Abs` — instead, small syntactic tricks encode the mode.

### Singleton types for index registers

```haskell
data X_ = X
data Y_ = Y
```

These are values, not types. They appear in tuple and operator expressions to indicate indexed and indirect modes.

### The `#` operator for immediate mode

```haskell
(#) :: (Imm -> ASM ()) -> Word8 -> ASM ()
f # v = f (Imm v)
```

The `Imm` newtype still exists internally but the user never writes it. Usage:

```haskell
lda # 0x40         -- LDA #$40
cmp # 0x80         -- CMP #$80
```

The space before `#` is syntactically required in Haskell.

### Bare typed variables for zero-page mode

User-defined variables carry their addressing mode in their type:

```haskell
newtype Var8  = Var8  Word8    -- 8-bit variable in zero page
newtype Var16 = Var16 Word8    -- 16-bit variable (2 consecutive ZP bytes)
data    Ptr   = Ptr   Word8    -- 16-bit pointer in zero page
```

These get `AutoAddr` instances so they can be passed directly to instructions:

```haskell
instance AutoAddr Var8 where
  lda (Var8 a) = emit [0xA5, a]     -- LDA zp
  sta (Var8 a) = emit [0x85, a]     -- STA zp
  ...
```

Usage:

```haskell
playerX <- allocZP    -- returns Var8
lda playerX           -- LDA $FB   (zero page, no constructor noise)
sta playerX           -- STA $FB
```

### Tuple syntax for indexed modes

Comma-separated tuples with X or Y indicate indexed addressing:

```haskell
instance AutoAddr (Word8, X_)  where ...  -- zero page,X
instance AutoAddr (Word8, Y_)  where ...  -- zero page,Y
instance AutoAddr (Word16, X_) where ...  -- absolute,X
instance AutoAddr (Word16, Y_) where ...  -- absolute,Y
```

Usage:

```haskell
lda (0xFB, X)          -- LDA $FB,X     zero page,X
sta (0x0400, X)        -- STA $0400,X   absolute,X
lda (0x0300, Y)        -- LDA $0300,Y   absolute,Y
```

Zero-page vs absolute is inferred from `Word8` vs `Word16`. If the user passes a raw numeric literal, a type annotation may be needed for absolute mode: `(0x0400 :: Word16, X)`.

### The `!` operator for indirect modes

The bang operator `!` signals indirection. A `Word8` (or `Ptr`) on the left and an index register on the right produce indirect addressing:

```haskell
class Indirectable a ix result | a ix -> result where
  (!) :: a -> ix -> result

instance Indirectable Word8 Y_ IndirectY where
  a ! Y = IndirectY a

instance Indirectable Word8 X_ IndirectX where
  a ! X = IndirectX a

instance Indirectable Ptr Y_ IndirectY where
  (Ptr a) ! Y = IndirectY a

instance Indirectable Ptr X_ IndirectX where
  (Ptr a) ! X = IndirectX a
```

`IndirectY` and `IndirectX` are internal newtypes with appropriate `AutoAddr` instances:

```haskell
newtype IndirectY = IndirectY Word8
newtype IndirectX = IndirectX Word8

instance AutoAddr IndirectY where
  lda (IndirectY a) = emit [0xB1, a]    -- LDA ($zp),Y
  sta (IndirectY a) = emit [0x91, a]    -- STA ($zp),Y

instance AutoAddr IndirectX where
  lda (IndirectX a) = emit [0xA1, a]    -- LDA ($zp,X)
  sta (IndirectX a) = emit [0x81, a]    -- STA ($zp,X)
```

Usage:

```haskell
lda (0xFB ! Y)         -- LDA ($FB),Y   indirect,Y
sta (0xFB ! X)         -- STA ($FB,X)   indirect,X
lda (src ! Y)          -- LDA ($FB),Y   using a Ptr variable
```

### Visual mnemonic

The distinction between comma and bang mirrors the 6502's distinction between indexed and indirect:

```
Comma = indexed:    lda (addr, X)     →  LDA addr,X
Bang  = indirect:   lda (addr ! X)    →  LDA (addr,X)  or  LDA (addr),Y
```

### Note on `!` conflicts

`!` is exported by some standard modules (e.g. `Data.Array`). Hide it in the import:

```haskell
import Prelude hiding ((!))
```

Or, if that proves too disruptive, use an alternative operator like `!>` or `.!`.

---

## Part 3: The AutoAddr Typeclass Hierarchy

### Separate classes for different instruction families

Not all instructions support all addressing modes. Model this by having separate typeclasses for different instruction groups. The 6502's instructions naturally cluster:

```haskell
class Loadable op where
  lda :: op -> ASM ()
  ldx :: op -> ASM ()
  ldy :: op -> ASM ()

class Storable op where
  sta :: op -> ASM ()
  stx :: op -> ASM ()
  sty :: op -> ASM ()

class ALUop op where
  adc :: op -> ASM ()
  sbc :: op -> ASM ()
  and_ :: op -> ASM ()   -- underscore to avoid Prelude conflict
  ora :: op -> ASM ()
  eor :: op -> ASM ()

class Comparable op where
  cmp :: op -> ASM ()
  cpx :: op -> ASM ()
  cpy :: op -> ASM ()

class RMWop op where
  inc :: op -> ASM ()
  dec :: op -> ASM ()
  asl :: op -> ASM ()
  lsr :: op -> ASM ()
  rol :: op -> ASM ()
  ror :: op -> ASM ()
```

The `Imm` type gets `Loadable`, `ALUop`, `Comparable` instances but NOT `Storable` or `RMWop` — you can't `STA #$40` or `INC #$40`. This is enforced at compile time.

### Implied/accumulator mode instructions

Instructions with no operand are just plain functions, no typeclass needed:

```haskell
inx, iny, dex, dey :: ASM ()
tax, tay, txa, tya, tsx, txs :: ASM ()
pha, pla, php, plp :: ASM ()
clc, sec, cli, sei, cld, sed, clv :: ASM ()
nop, brk, rti, rts :: ASM ()

-- Accumulator mode (ASL A, LSR A, ROL A, ROR A):
-- Option 1: separate names
asla, lsra, rola, rora :: ASM ()
-- Option 2: use a singleton
data A_ = A
instance RMWop A_ where
  asl A = emit [0x0A]
  lsr A = emit [0x4A]
  ...
-- Usage: asl A, lsr A
```

The `A` singleton approach is nice because `asl A` reads like real assembly `ASL A`.

---

## Part 4: Zero-Page Allocation

### Allocator state

Extend the ASM monad (or layer a StateT over it) to track available zero-page locations:

```haskell
data ASMState = ASMState
  { asmOffset    :: Word16
  , asmOutput    :: Endo [Word8]
  , asmFreeZP    :: [Word8]          -- pool of free ZP addresses
  }
```

Initialize with the C64-safe zero-page locations (avoiding those claimed by BASIC/Kernal):

```haskell
c64FreeZP :: [Word8]
c64FreeZP = [0x02] ++ [0x04..0x05] ++ [0x06..0x09]
         ++ [0x0B..0x0C] ++ [0x0E..0x2F]
         ++ [0xFB..0xFE]
-- Expand this list based on whether BASIC ROM is banked out.
```

### Allocation functions

```haskell
allocZP :: ASM Var8
allocZP = do
  (a:rest) <- gets asmFreeZP
  modify $ \s -> s { asmFreeZP = rest }
  pure (Var8 a)

allocZP16 :: ASM Var16
allocZP16 = do
  (a:b:rest) <- gets asmFreeZP
  -- Assert a and b are consecutive, or just take two and store
  -- the low-byte address:
  modify $ \s -> s { asmFreeZP = rest }
  pure (Var16 a)

allocPtr :: ASM Ptr
allocPtr = do
  (a:b:rest) <- gets asmFreeZP
  modify $ \s -> s { asmFreeZP = rest }
  pure (Ptr a)
```

Note: when using `MonadFix`, the allocator must be careful about laziness. Allocation is a side effect that must happen eagerly (it determines how many ZP bytes remain). This is fine as long as the *result* (the Var8/Ptr value) is what's used lazily in forward references, not the allocation action itself.

---

## Part 5: Data Embedding Primitives

```haskell
byte :: [Word8] -> ASM ()
byte = emit

word :: [Word16] -> ASM ()
word = traverse_ (\w -> emit [lo w, hi w])

-- PETSCII string (C64 character encoding)
petscii :: String -> ASM ()
petscii = byte . map charToPetscii

-- Null-terminated PETSCII string
pstring :: String -> ASM ()
pstring s = petscii s >> byte [0x00]

-- Include external binary file (sprites, SID music, charset)
incbin :: FilePath -> ASM ()
incbin path = do
  bytes <- liftIO (BS.readFile path)
  emit (BS.unpack bytes)

-- Computed lookup tables (a major strength of the EDSL)
--
-- Example: 256-entry sine table
-- sineTable <- here
-- byte [round (128 + 127 * sin (2 * pi * i / 256)) | i <- [0..255]]
```

### Alignment

```haskell
align :: Int -> ASM ()
align n = do
  pos <- here
  let padding = (n - fromIntegral pos `mod` n) `mod` n
  emit (replicate padding 0x00)
```

---

## Part 6: Memory Segments and Layout

### Segment model

Programs need non-contiguous memory regions. Define segments as labeled regions with constraints:

```haskell
data Region = Region
  { regionBase  :: Word16
  , regionLimit :: Word16
  , regionFill  :: Maybe Word8   -- Nothing = virtual (no bytes emitted)
  }

org :: Word16 -> ASM ()
-- Sets the current assembly address. Pads with fill bytes if needed.
```

### Standard C64 regions

```haskell
c64Code    = Region 0x0810 0xCFFF (Just 0x00)
c64Sprites = Region 0x2000 0x3FFF (Just 0x00)
c64ZPVars  = Region 0x0002 0x00FF Nothing
c64Music   = Region 0xC000 0xCFFF (Just 0x00)
```

### BASIC stub (auto-run boilerplate)

```haskell
basicStub :: Word16 -> ASM ()
-- Emits the standard BASIC stub at $0801:
--   0801: 0B 08       pointer to next line
--   0803: E5 07       line number (2021 or similar)
--   0805: 9E          SYS token
--   0806: 32 30 36 34 "2064" (ASCII digits of entry address)
--   080A: 00          end of line
--   080B: 00 00       end of program
-- Then sets org to the entry address.
```

---

## Part 7: Control Flow Combinators

Built on top of `MonadFix` labels:

```haskell
if_eq :: ASM () -> ASM () -> ASM ()
if_eq thenBlock elseBlock = mdo
  bne elseStart
  thenBlock
  jmp end
  elseStart <- here
  elseBlock
  end <- here
  pure ()

if_ :: (Word16 -> ASM ()) -> ASM () -> ASM () -> ASM ()
-- Generalized: takes any branch instruction as first argument.
-- Example: if_ bcc thenBlock elseBlock

while_ :: ASM () -> ASM () -> ASM ()
while_ cond body = mdo
  top <- here
  cond
  beq exit
  body
  jmp top
  exit <- here
  pure ()

for_x :: Word8 -> ASM () -> ASM ()
-- Idiomatic 6502 loop: LDX #n, body, DEX, BNE loop
for_x count body = mdo
  ldx # count
  loop <- here
  body
  dex
  bne loop
```

---

## Part 8: 16-bit Operation Library

These are regular Haskell functions that emit sequences of 6502 instructions. They operate on `Var16` and `Ptr` typed variables:

```haskell
add16 :: Var16 -> Var16 -> Var16 -> ASM ()
-- dst = a + b using CLC/ADC chain on low and high bytes

sub16 :: Var16 -> Var16 -> Var16 -> ASM ()
-- dst = a - b using SEC/SBC chain

inc16 :: Var16 -> ASM ()
-- Increment 16-bit value: INC lo; BNE skip; INC hi; skip:

cmp16 :: Var16 -> Var16 -> ASM ()
-- Compare two 16-bit values, sets carry/zero for subsequent branch

mov16 :: Var16 -> Var16 -> ASM ()
-- Copy 16-bit value
```

---

## Part 9: Output and Debug Support

### .prg output

```haskell
writePRG :: FilePath -> Word16 -> [Word8] -> IO ()
-- Writes a C64 .prg file: 2-byte little-endian load address header
-- followed by the program bytes.
```

### VICE monitor label export

```haskell
exportViceLabels :: FilePath -> [(String, Word16)] -> IO ()
-- Writes a VICE .vs symbol file:
--   al C:0810 .main
--   al C:0820 .loop
--   al C:083F .clearScreen
-- This lets the VICE debugger show symbolic names.
```

### Compile-time assertions

```haskell
fitsIn :: Int -> ASM () -> ASM ()
-- Asserts that the block emits at most N bytes. Errors at assembly time.

samePage :: Word16 -> ASM ()
-- Asserts current offset is on the same 256-byte page as the argument.
```

### Source listing

```haskell
annotate :: String -> ASM () -> ASM ()
-- Associates a label string with a code region for listing output.
-- The listing maps addresses to disassembled instructions with annotations.
```

---

## Part 10: Required GHC Extensions

```haskell
{-# LANGUAGE RecursiveDo #-}             -- for mdo blocks (MonadFix labels)
{-# LANGUAGE MultiParamTypeClasses #-}   -- for Indirectable a ix result
{-# LANGUAGE FunctionalDependencies #-}  -- for | a ix -> result
{-# LANGUAGE FlexibleInstances #-}       -- for instances on tuples, Word8, etc.
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for deriving Num on newtypes
{-# LANGUAGE TypeFamilies #-}            -- optional, for Mode type family
```

---

## Part 11: Complete Addressing Mode Syntax Reference

| Real 6502 syntax  | EDSL syntax               | Addressing mode  |
|--------------------|---------------------------|------------------|
| `LDA #$40`         | `lda # 0x40`              | Immediate        |
| `LDA $FB`          | `lda playerX`             | Zero page (var)  |
| `LDA $FB`          | `lda (0xFB :: Word8)`     | Zero page (raw)  |
| `LDA $0400`        | `lda (0x0400 :: Word16)`  | Absolute         |
| `LDA $FB,X`        | `lda (0xFB, X)`           | Zero page,X      |
| `LDA $FB,Y`        | `lda (0xFB, Y)`           | Zero page,Y      |
| `LDA $0400,X`      | `lda (0x0400, X)`         | Absolute,X       |
| `LDA $0400,Y`      | `lda (0x0400, Y)`         | Absolute,Y       |
| `LDA ($FB,X)`      | `lda (0xFB ! X)`          | Indirect,X       |
| `LDA ($FB),Y`      | `lda (0xFB ! Y)`          | Indirect,Y       |
| `LDA ($FB),Y`      | `lda (src ! Y)`           | Indirect,Y (ptr) |
| `ASL A`            | `asl A`                   | Accumulator      |
| `JMP ($FFFC)`      | `jmpInd (0xFFFC)`         | Indirect (JMP)   |

---

## Part 12: Complete Example Program

This is what a finished program should look like using the EDSL:

```haskell
{-# LANGUAGE RecursiveDo #-}
import Prelude hiding ((!))
import MOS6502

main :: IO ()
main = do
  let prg = assemble c64Default $ mdo

        basicStub entry

        entry <- here

        -- clear screen with spaces
        lda # 0x20
        for_x 0x00 $ do
          sta (0x0400, X)
          sta (0x0500, X)
          sta (0x0600, X)
          sta (0x0700, X)

        -- set up source pointer
        lda # lo messageAddr
        sta src
        lda # hi messageAddr
        sta (src + 1)

        -- print message
        ldy # 0x00
        printLoop <- here
        lda (src ! Y)
        beq done
        sta (0x0400, Y)
        iny
        bne printLoop

        done <- here
        rts

        -- data
        messageAddr <- here
        petscii "HELLO FROM HASKELL"
        byte [0x00]

        -- variables
        src <- allocPtr

  writePRG "hello.prg" prg

```

---

## Implementation Order

1. **ASM monad with MonadFix** — the foundation everything else builds on
2. **emit, here, branch, jmp** — minimal instruction emission
3. **Opcode functions with AutoAddr typeclass** — all 56 instructions, all addressing modes
4. **The `#` operator, tuple instances, `!` operator** — the syntactic sugar layer
5. **Typed variables (Var8, Var16, Ptr) and ZP allocator** — named variables
6. **Data embedding** (byte, word, petscii, incbin, align)
7. **BASIC stub and .prg output** — now you can run programs on a C64
8. **Control flow combinators** (if_, while_, for_x)
9. **16-bit operation library** (add16, sub16, cmp16, etc.)
10. **Debug output** (VICE labels, listings, assertions)

Each stage produces a usable tool. After step 7, you can write and run real C64 programs. Everything after that is ergonomic improvement.
