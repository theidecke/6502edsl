# MOS 6502 Tagless-Final EDSL — Design Document

## 1. Vision and Goals

This project builds an embedded domain-specific language in Haskell for programming the MOS 6502 processor, targeting the Commodore 64. The EDSL is the foundation for a larger effort: growing from an assembler upward through typed variables, control flow, an expression language, and eventually an optimizing compiler for a high-level language — connected by the Futamura projections.

### Primary design goals

**G1 — Dual interpretation from a shared definition.** Every 6502 instruction is defined once in a tagless-final typeclass and interpreted two ways: the assembler emits bytes, the emulator executes operations on CPU state. Both interpretations are driven from the same source expression, making disagreement structurally impossible.

**G2 — Full laziness throughout the pipeline.** The assembler produces a lazy byte stream where label addresses are thunks. The emulator's RAM is a lazy persistent data structure. The execution trace is a lazy list of CPU states. Observation through lenses forces only the minimal computation needed to answer the question asked. Assembly, emulation, and observation form a single demand-driven pipeline.

**G3 — Property-based correctness.** The dual interpretation enables differential testing: assemble a snippet to bytes, load and execute them in the emulator, compare the result against direct interpretation of the same snippet. QuickCheck generators produce random instruction sequences and random initial states. Every macro (16-bit arithmetic, control flow) is tested this way.

**G4 — Ergonomic syntax.** The EDSL reads as close to real 6502 assembly as Haskell allows. Addressing modes are encoded in the types of operands through typeclasses, tuples, and operators — not through explicit constructors. Named variables replace raw addresses. MonadFix provides forward label references.

**G5 — Efficient persistent state for traces.** CPU states across the execution trace share unchanged memory through a nibble-keyed trie. Writing one byte allocates ~512 bytes of new nodes while sharing the rest of the 64KB address space. Lenses focus on specific registers or addresses across the trace without forcing unrelated computation.

**G6 — Foundation for the Futamura projections.** The architecture separates static information (known at assembly/compile time) from dynamic information (known only at runtime). The tagless-final boundary is the staging boundary: different typeclass instances produce interpreters, compilers, and partial evaluators from the same source program.

---

## 2. Foundational Concepts

### 2.1 Tagless-final style

The core insight is that a program's meaning is determined by the *algebra* (typeclass instance) it's interpreted in, not by a fixed representation. The same expression `lda # 0x40 >> sta playerX` can mean "emit bytes A9 40 85 FB" (assembler) or "set register A to 0x40, then write 0x40 to address 0xFB in RAM" (emulator). The program is polymorphic over `m`; the instance of `m` determines the interpretation.

This is the manual first Futamura projection: the typeclass boundary separates what is specialized away (the choice of interpretation) from what remains (the program's structure).

### 2.2 Staging: layout vs. execution

A 6502 program has two phases with fundamentally different concerns.

**Phase 1 — Layout (static).** What goes where. Label addresses, zero-page allocation, segment placement, alignment. No runtime values flow; only addresses are computed. This phase runs once at assembly time and produces a fully resolved instruction stream.

**Phase 2 — Execution (dynamic).** Registers change, memory is read and written, branches are taken or not. This phase runs per-input and operates on the resolved instructions from Phase 1.

The assembler lives entirely in Phase 1. The emulator lives entirely in Phase 2. Both need the *results* of Phase 1 (resolved addresses), but the resolution itself happens before either interpreter sees the instructions. The resolved `[Instruction]` stream is the pivot point between the two phases.

### 2.3 MonadFix and lazy label resolution

Forward references in assembly are resolved via Haskell's `MonadFix`, which ties a lazy knot: a label's value is a thunk that evaluates to the byte offset computed by a later `here` action. This works because instruction *sizes* are always known eagerly (determined by opcode and addressing mode), while operand *values* (including branch targets) can be lazy. The size/content separation is the invariant that makes MonadFix safe for assembly.

### 2.4 Derivatives, lenses, and one-hole contexts

An instruction with an unresolved address is the *derivative* of the Instruction type with respect to the address field — a structure with exactly one hole. Filling the hole (providing the address) is integration. A lens `Lens' Instruction Word16` is exactly this derivative packaged as a composable accessor.

The same principle applies to the execution trace: a lens into one register at one timestep is a one-hole context in the `[CPUState]` structure. Laziness ensures that computing the derivative (observing through the lens) forces only the part of the structure needed to extract the focused value. Unobserved parts — other registers, other timesteps, other memory addresses — remain as unevaluated thunks.

### 2.5 Persistent data structures and structural sharing

The execution trace is a lazy list of CPU states. Naively, each state would copy 64KB of RAM. Instead, RAM is a persistent nibble-keyed trie where writes create new nodes along one path (4 nodes, ~512 bytes) while sharing all other subtrees with the previous state. The `Default` constructor represents uninitialized memory without allocating leaf nodes.

Diffing two trie states uses pointer equality: if two nodes are the same pointer, their entire subtrees are identical, and the diff skips them. For a typical instruction that writes 0–1 bytes, diffing traverses only 4 levels of the changed path and skips everything else.

---

## 3. Module Structure

```
MOS6502/
├── ISA.hs                  -- Instruction set data types
├── Class.hs                -- Tagless-final typeclasses (the shared interface)
├── Syntax.hs               -- Operators and typeclass instances for ergonomic syntax
├── Asm.hs                  -- Assembler interpreter
├── Emu.hs                  -- Emulator interpreter
├── RAM.hs                  -- Persistent lazy nibble trie
├── Trace.hs                -- Lazy execution traces with lens access
├── Macro.hs                -- 16-bit operations, control flow combinators
├── C64.hs                  -- BASIC stub, memory map, .prg output, VICE labels
├── Test.hs                 -- QuickCheck generators, differential testing
└── Semantics.hs            -- High-level language semantics (Futamura projections)
```

### 3.1 ISA — Instruction set data types

**Depends on:** nothing.

This module is the single source of truth for the 6502 instruction set. It defines fully resolved instructions (no holes, no thunks) as algebraic data types, plus encoding/decoding functions and a cycle cost table.

```haskell
data Opcode = LDA | STA | ADC | SBC | AND | ORA | EOR
            | CMP | CPX | CPY | LDX | LDY | STX | STY
            | INC | DEC | ASL | LSR | ROL | ROR
            | JMP | JSR | RTS | RTI | BRK | NOP
            | BEQ | BNE | BCC | BCS | BPL | BMI | BVS | BVC
            | CLC | SEC | CLI | SEI | CLD | SED | CLV
            | TAX | TAY | TXA | TYA | TSX | TXS
            | PHA | PLA | PHP | PLP
            | INX | INY | DEX | DEY
            deriving (Eq, Show, Enum, Bounded)

data AddrMode
  = Implied
  | Accumulator
  | Immediate   Word8
  | ZeroPage    Word8
  | ZeroPageX   Word8
  | ZeroPageY   Word8
  | Absolute    Word16
  | AbsoluteX   Word16
  | AbsoluteY   Word16
  | IndirectX   Word8       -- (zp,X)
  | IndirectY   Word8       -- (zp),Y
  | Relative    Int8        -- branch offset (resolved)
  | Indirect    Word16      -- JMP ($addr) only
  deriving (Eq, Show)

data Instruction = Instruction Opcode AddrMode
  deriving (Eq, Show)

-- Encoding: Instruction → [Word8]
encode :: Instruction -> [Word8]

-- Decoding: Word8 stream → Instruction
decode :: Word8 -> (Int, Word8 -> Word8 -> Instruction)
  -- Returns (additional bytes needed, continuation)

-- Cost model: Instruction → base cycles (without page-cross penalty)
baseCycles :: Instruction -> Int

-- Size: Instruction → number of bytes
instrSize :: Instruction -> Int

-- Traversal over the address field (the "derivative")
instrTarget :: Traversal' Instruction Word16
```

The `instrTarget` traversal focuses on the address field of branch and jump instructions, providing the derivative/lens interface for label resolution.

### 3.2 Class — Tagless-final typeclasses

**Depends on:** ISA (for Var8, Var16, Ptr types, Register, Flag).

This module defines the shared interface that both interpreters implement. It is split into layers based on which interpreter uses which operations.

```haskell
-- === Layer 1: CPU instructions (both assembler and emulator) ===

class Monad m => Loadable m op where
  lda :: op -> m ()
  ldx :: op -> m ()
  ldy :: op -> m ()

class Monad m => Storable m op where
  sta :: op -> m ()
  stx :: op -> m ()
  sty :: op -> m ()

class Monad m => ALUop m op where
  adc :: op -> m ()
  sbc :: op -> m ()
  and_ :: op -> m ()
  ora :: op -> m ()
  eor :: op -> m ()

class Monad m => Comparable m op where
  cmp :: op -> m ()
  cpx :: op -> m ()
  cpy :: op -> m ()

class Monad m => RMWop m op where
  inc :: op -> m ()
  dec :: op -> m ()
  asl_ :: op -> m ()
  lsr_ :: op -> m ()
  rol :: op -> m ()
  ror :: op -> m ()

-- Implied and accumulator mode instructions: plain methods
class Monad m => ImpliedOps m where
  inx, iny, dex, dey :: m ()
  tax, tay, txa, tya, tsx, txs :: m ()
  pha, pla, php, plp :: m ()
  clc, sec, cli, sei, cld, sed, clv :: m ()
  nop, brk, rti, rts :: m ()

-- Accumulator mode RMW via singleton
data A_ = A

-- Branch and jump
class Monad m => BranchOps m where
  beq, bne, bcc, bcs, bpl, bmi, bvs, bvc :: Word16 -> m ()
  jmp :: Word16 -> m ()
  jsr :: Word16 -> m ()
  jmpInd :: Word16 -> m ()

-- === Layer 2: Assembler-only operations ===

class (Monad m) => AsmDSL m where
  here      :: m Word16       -- current program counter / byte offset
  org       :: Word16 -> m () -- set assembly origin
  emit      :: [Word8] -> m () -- raw byte emission
  allocZP   :: m Var8         -- allocate an 8-bit zero-page variable
  allocZP16 :: m Var16        -- allocate a 16-bit zero-page variable (2 bytes)
  allocPtr  :: m Ptr          -- allocate a 16-bit zero-page pointer (2 bytes)
  align     :: Int -> m ()    -- pad to alignment boundary

-- === Layer 3: Emulator-only operations ===

class (Monad m) => EmuDSL m where
  getReg   :: Register -> m Word8
  setReg   :: Register -> Word8 -> m ()
  getPC    :: m Word16
  readMem  :: Word16 -> m Word8
  writeMem :: Word16 -> Word8 -> m ()
  getCycles :: m Int

-- === Layer 4: Testing assertions (emulator only) ===

class (EmuDSL m) => TestDSL m where
  assertA    :: Word8 -> m ()
  assertX    :: Word8 -> m ()
  assertY    :: Word8 -> m ()
  assertMem  :: Word16 -> Word8 -> m ()
  assertFlag :: Flag -> Bool -> m ()
```

### 3.3 Syntax — Operators and addressing mode dispatch

**Depends on:** Class.

This module defines the syntactic sugar that makes the EDSL read like assembly. The user imports this module and gets the concise syntax; the internal addressing mode types remain hidden.

```haskell
-- === Internal addressing mode newtypes ===
-- These are NOT exported to the user.

newtype Imm       = Imm Word8
newtype IndirectX = IndirectX Word8
newtype IndirectY = IndirectY Word8

-- === Singleton index registers ===
-- These ARE exported.

data X_ = X
data Y_ = Y

-- === The # operator for immediate mode ===

(#) :: (Imm -> m ()) -> Word8 -> m ()
f # v = f (Imm v)

-- Usage: lda # 0x40

-- === Tuple instances for indexed modes ===
-- Comma = indexed (no indirection)

-- (Word8, X_)  → zero page,X
-- (Word8, Y_)  → zero page,Y
-- (Word16, X_) → absolute,X
-- (Word16, Y_) → absolute,Y

-- === The ! operator for indirect modes ===
-- Bang = indirect

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

-- === Typed zero-page variables ===

newtype Var8  = Var8  { var8Addr  :: Word8 }
newtype Var16 = Var16 { var16Addr :: Word8 }
data    Ptr   = Ptr   { ptrAddr   :: Word8 }
```

**Complete addressing mode syntax reference:**

| Real 6502 syntax | EDSL syntax              | Mode          |
|-------------------|--------------------------|---------------|
| `LDA #$40`        | `lda # 0x40`             | Immediate     |
| `LDA $FB`         | `lda playerX`            | Zero page     |
| `LDA $0400`       | `lda (0x0400 :: Word16)` | Absolute      |
| `LDA $FB,X`       | `lda (0xFB, X)`          | Zero page,X   |
| `LDA $FB,Y`       | `lda (0xFB, Y)`          | Zero page,Y   |
| `LDA $0400,X`     | `lda (0x0400, X)`        | Absolute,X    |
| `LDA $0400,Y`     | `lda (0x0400, Y)`        | Absolute,Y    |
| `LDA ($FB,X)`     | `lda (0xFB ! X)`         | Indirect,X    |
| `LDA ($FB),Y`     | `lda (0xFB ! Y)`         | Indirect,Y    |
| `LDA ($FB),Y`     | `lda (src ! Y)`          | Indirect,Y    |
| `ASL A`           | `asl_ A`                 | Accumulator   |
| `JMP ($FFFC)`     | `jmpInd 0xFFFC`          | Indirect jump |

**Visual mnemonic:** comma means indexed (`addr,X`), bang means indirect (`(addr),Y`). The two connectives mirror the 6502's actual syntactic distinction.

### 3.4 Asm — Assembler interpreter

**Depends on:** ISA, Class, Syntax.

Implements all the Layer 1 and Layer 2 typeclasses. Each CPU instruction emits the correct opcode bytes and advances the program counter. Supports `MonadFix` for forward label references.

```haskell
newtype Asm a = Asm (Word16 -> (a, Word16, Endo [Word8]))

data AsmState = AsmState
  { asmPC     :: !Word16        -- current byte offset (strict, drives sizing)
  , asmOutput :: Endo [Word8]   -- output bytes (lazy in operand values)
  , asmFreeZP :: [Word8]        -- available zero-page locations
  }

instance Functor Asm
instance Applicative Asm
instance Monad Asm

instance MonadFix Asm where
  mfix f = Asm $ \pos ->
    let (a, pos', out) = runAsm' (f a) pos
    in  (a, pos', out)
```

**Key invariant for MonadFix safety:** the `Word16 → Word16` component (size tracking) is always strict and eager. The `Endo [Word8]` component (byte content) is lazy in operand values. This means instruction sizes are computed immediately (enabling the program counter to advance correctly), while branch target bytes remain as thunks until the output is consumed.

The assembler does NOT interpret the semantics of instructions — it does not track register or flag state. Its only job is to compute addresses and emit bytes.

### 3.5 Emu — Emulator interpreter

**Depends on:** ISA, Class, Syntax, RAM.

Implements all the Layer 1 and Layer 3 typeclasses. Each CPU instruction modifies CPU state (registers, flags, RAM, program counter, cycle counter). RAM is the persistent nibble trie from the RAM module, enabling lazy evaluation and structural sharing.

```haskell
data CPUState = CPUState
  { _regA    :: !Word8
  , _regX    :: !Word8
  , _regY    :: !Word8
  , _regSP   :: !Word8
  , _regP    :: !Word8        -- status register (NV-BDIZC)
  , _regPC   :: !Word16
  , _ram     :: RAM            -- persistent lazy trie (not strict!)
  , _cycles  :: !Int
  }
makeLenses ''CPUState

data Register = RA | RX | RY | RSP | RP
data Flag = Carry | Zero | IRQ | Decimal | Break | Overflow | Negative
```

Registers and the cycle counter are strict (they change every instruction and must not accumulate thunks). RAM is lazy — cells that are never read remain as thunks from the assembly byte stream.

The emulator has two modes of operation:

**Direct interpretation mode:** instruction functions from the tagless-final class directly modify CPUState. Used for differential testing against the assembler.

**Fetch-decode-execute mode:** reads opcodes from RAM by program counter, decodes via ISA.decode, and dispatches. Used for executing assembled binaries. This mode exercises both the assembler's encoding and the emulator's decoding.

### 3.6 RAM — Persistent lazy nibble trie

**Depends on:** nothing (standalone data structure).

A 4-level trie keyed on nibbles of a 16-bit address. Each level branches 16 ways. Leaves hold individual bytes. The `Default` constructor represents uninitialized subtrees without allocating nodes.

```haskell
data Trie
  = Node16 {-# UNPACK #-} !(SmallArray Trie)
                       -- 16 children, 128 bytes per node
  | Leaf   !Word8      -- a single byte value
  | Default            -- entire subtree is all zeros

-- Address decomposition by nibbles:
-- Word16 = nibble3 : nibble2 : nibble1 : nibble0
-- Level 0 (root): branches on bits 15–12
-- Level 1:        branches on bits 11–8
-- Level 2:        branches on bits 7–4
-- Level 3:        branches on bits 3–0
```

**Core operations:**

| Operation         | Algorithm                              | Cost            | Allocation     |
|-------------------|----------------------------------------|-----------------|----------------|
| `readByte`        | 4 nibble lookups, Default → 0x00       | O(1) (4 hops)   | 0              |
| `writeByte`       | Copy-on-write along 4-node path        | O(1) (4 copies) | ~512 bytes     |
| `fromLazyList`    | Bottom-up trie construction            | O(n)            | ~4n bytes      |
| `diffRAM`         | Walk both tries, skip shared pointers  | O(changed)      | 0              |
| `empty`           | Single `Default` node                  | O(1)            | 0              |

**Structural sharing diagram for a write to address $0402:**

```
     Before:                     After:

     [Root]                      [Root']          ← 1 new node (128B)
    / |    \                    /  |    \
  ... [4]  ...     →        ... [4']   ...        ← 1 new node (128B)
       |                         |                   15 children shared
      [0]                       [0']               ← 1 new node (128B)
       |                         |                   15 children shared
   [0..F]                    [0..F']               ← 1 new node (128B)
      ^                         ^                    15 children shared
   old byte                  new byte

Total new allocation: 4 × 128B = 512 bytes
Total shared: entire remaining 64KB address space
```

**Lazy initialization:** `fromLazyList` builds the trie structure eagerly (determined by addresses) but leaf values are thunks into the assembler's lazy byte stream. A cell that the emulator never reads is never forced, and the corresponding label resolution in the assembler never triggers.

**Diffing with pointer equality:** `diffRAM` compares two tries by walking them in parallel. When two children are the same pointer (detected via `sameSmallArray` / `reallyUnsafePtrEquality#`), their entire subtrees are identical and the diff skips them immediately. For a write of 1 byte, the diff traverses 4 levels on the changed path and skips 15 × 4 = 60 subtrees.

**Lenses into RAM:**

```haskell
memAt :: Word16 -> Lens' Trie Word8
memAt addr = lens (readByte addr) (\trie val -> writeByte addr val trie)
```

This composes with the CPUState lens: `cpuState ^. ram . memAt 0x0400` reads one byte, forcing exactly the 4 trie nodes on the path to that address.

### 3.7 Trace — Lazy execution traces with lens access

**Depends on:** Emu, RAM.

Provides lazy, demand-driven execution traces and lens-based observation.

```haskell
-- The core trace: a lazy infinite list of CPU states
trace :: CPUState -> [CPUState]
trace s = s : trace (step s)

-- step is pure: returns a new CPUState sharing RAM with the old one
step :: CPUState -> CPUState
```

**Demand propagation chain when observing a trace:**

```
Lens view (e.g., view regA at step 50)
  │ forces trace !! 50
  ▼
CPUState after 50 steps
  │ forces step function for each of the 50 instructions
  ▼
Each step reads RAM at current PC
  │ forces the trie lookup at that address
  ▼
Trie leaf may be a thunk from the assembler's byte stream
  │ if this byte encodes a branch target, forces the address thunk
  ▼
MonadFix resolves: the thunk evaluates to the byte offset
  │ this forces instruction SIZES (always eager) but not other CONTENTS
  ▼
Resolution complete — only the demanded bytes were computed
```

**Observation combinators:**

```haskell
-- Watch a register evolve over time
watchReg :: Lens' CPUState Word8 -> [CPUState] -> [Word8]
watchReg l = map (view l)

-- Watch a 16-bit zero-page variable
watch16 :: Word8 -> Getter CPUState Word16
watch16 zpAddr = to $ \s ->
  let lo = s ^. ram . memAt (fromIntegral zpAddr)
      hi = s ^. ram . memAt (fromIntegral zpAddr + 1)
  in  fromIntegral hi `shiftL` 8 .|. fromIntegral lo

-- Diff between consecutive states
deltas :: [CPUState] -> [[(Word16, Word8, Word8)]]
deltas states = zipWith (\a b -> diffRAM (a ^. ram) (b ^. ram))
                        states (tail states)

-- Run until a condition is met
runUntil :: (CPUState -> Bool) -> CPUState -> CPUState
runUntil pred = head . dropWhile (not . pred) . trace
```

Because everything is lazy, `take 10 (watchReg regA (trace s0))` forces exactly 10 emulation steps, which force exactly the RAM reads needed by those 10 instructions, which force exactly the assembly bytes at those addresses. Nothing else is computed.

### 3.8 Macro — 16-bit operations, control flow combinators

**Depends on:** Class (polymorphic over any `m` with the right instances).

All macros are polymorphic — they work in any monad that implements the required typeclasses. This means they are automatically testable through both interpreters.

```haskell
-- 16-bit arithmetic
add16 :: (Loadable m Var8, Storable m Var8, ALUop m Var8,
          ImpliedOps m)
      => Var16 -> Var16 -> Var16 -> m ()

sub16 :: (...) => Var16 -> Var16 -> Var16 -> m ()
inc16 :: (...) => Var16 -> m ()
cmp16 :: (...) => Var16 -> Var16 -> m ()
mov16 :: (...) => Var16 -> Var16 -> m ()

-- Storing 16-bit immediate values
store16 :: (...) => Var16 -> Word16 -> m ()

-- Control flow (requires AsmDSL for labels via MonadFix)
if_    :: (AsmDSL m, BranchOps m, MonadFix m)
       => (Word16 -> m ()) -> m () -> m () -> m ()
while_ :: (AsmDSL m, BranchOps m, MonadFix m)
       => m () -> m () -> m ()
for_x  :: (AsmDSL m, BranchOps m, ImpliedOps m, Loadable m Imm, MonadFix m)
       => Word8 -> m () -> m ()
```

### 3.9 C64 — Target-specific support

**Depends on:** Asm, ISA.

```haskell
data TargetConfig = TargetConfig
  { freeZeroPage :: [Word8]
  , codeStart    :: Word16
  , ioRange      :: (Word16, Word16)   -- treated as volatile
  , availableRAM :: [(Word16, Word16)]
  }

c64Default :: TargetConfig

-- BASIC stub emission
basicStub :: Word16 -> Asm ()

-- .prg binary output (2-byte load address header + program bytes)
writePRG :: FilePath -> Word16 -> [Word8] -> IO ()

-- VICE monitor label file export
exportViceLabels :: FilePath -> [(String, Word16)] -> IO ()

-- Data embedding
byte    :: [Word8] -> Asm ()
word    :: [Word16] -> Asm ()
petscii :: String -> Asm ()
pstring :: String -> Asm ()
incbin  :: FilePath -> Asm ()

-- Compile-time assertions
fitsIn   :: Int -> Asm () -> Asm ()
samePage :: Word16 -> Asm ()
```

### 3.10 Test — QuickCheck generators and differential testing

**Depends on:** ISA, Class, Asm, Emu, Macro.

The testing strategy centers on the differential property: both paths through the architecture (direct interpretation and assemble-then-execute) must produce identical CPU state.

```haskell
-- Reified instruction for generation and shrinking
data TestInstr
  = TLDA_Imm Word8 | TLDA_ZP Word8
  | TSTA_ZP Word8
  | TADC_Imm Word8 | TSBC_Imm Word8
  | TINX | TDEX | TINY | TDEY
  | TCLC | TSEC
  | TAND_Imm Word8 | TORA_Imm Word8 | TEOR_Imm Word8
  -- ... covering all testable instruction forms
  deriving (Show)

-- Execute in both interpreters, reusing the same snippet
interpret :: MOS6502 m => [TestInstr] -> m ()

-- Generators
genInstr :: Gen TestInstr
genSnippet :: Gen [TestInstr]

-- Core differential property
prop_asmMatchesEmu :: Property
prop_asmMatchesEmu = forAll genSnippet $ \snippet ->
  let bytes        = runAsm (interpret snippet)
      stateFromAsm = execBytes 0x0810 bytes
      stateFromEmu = execDirect (interpret snippet)
  in  cpuRegsEqual stateFromAsm stateFromEmu

-- 16-bit macro property
prop_add16_correct :: Word16 -> Word16 -> Property
prop_add16_correct a b = ...
  -- Allocate vars, store a and b, run add16, check result == a + b

-- Property with random initial RAM
prop_randomRAM :: Property
prop_randomRAM = forAll arbitrary $ \initRAM ->
  forAll genSnippet $ \snippet ->
    -- ... both interpreters agree starting from the same random RAM
```

### 3.11 Semantics — High-level language interface (Futamura projections)

**Depends on:** Class, ISA, Asm, Emu.

This module defines the abstraction for the future high-level source language. It provides the staging boundary for partial evaluation.

```haskell
-- The high-level semantics, parameterized by interpretation
class Monad m => Semantics m where
  type Val m                              -- what a "value" is
  litInt   :: Word8 -> m (Val m)          -- literal constant
  add      :: Val m -> Val m -> m (Val m) -- arithmetic
  load     :: Addr -> m (Val m)           -- memory read
  store    :: Addr -> Val m -> m ()       -- memory write
  branch   :: Val m -> m () -> m () -> m ()  -- conditional

-- Instance 1: Concrete interpreter (Val = Word8)
-- Operations directly modify CPUState via EmuDSL
-- This is the standard interpreter.

-- Instance 2: Code generator (Val = Expr)
-- Static values (Lit) are folded at compile time.
-- Dynamic values (Dynamic) emit 6502 instructions.
-- This is the first Futamura projection.

data Expr
  = Lit Word8           -- known at compile time → constant fold
  | Dynamic Operand     -- known only at runtime → emit instructions

-- Instance 3: Reified free monad (Val = Symbolic)
-- Operations become data constructors, enabling generic partial evaluation.
-- This enables the second Futamura projection.
```

---

## 4. Data Structures

### 4.1 Addressing mode types (internal)

| Type        | Represents            | User syntax       | Exported? |
|-------------|-----------------------|--------------------|-----------|
| `Imm`       | Immediate value       | `# 0x40`           | No        |
| `Var8`      | 8-bit ZP variable     | `playerX`          | Yes       |
| `Var16`     | 16-bit ZP variable    | `score`            | Yes       |
| `Ptr`       | 16-bit ZP pointer     | `src`              | Yes       |
| `X_` / `Y_` | Index register tags   | `X` / `Y`         | Yes       |
| `A_`        | Accumulator tag       | `A`                | Yes       |
| `IndirectX` | (zp,X) mode           | `addr ! X`         | No        |
| `IndirectY` | (zp),Y mode           | `addr ! Y`         | No        |
| Tuples      | Indexed modes         | `(addr, X)`        | Implicit  |

### 4.2 Instruction (resolved)

Defined in ISA.hs. A flat ADT with no thunks or holes. This is the interchange format between the assembler (which produces it) and the emulator (which consumes it).

### 4.3 CPUState

Registers are strict. RAM is lazy (persistent trie). The cycle counter is strict. Lenses provide access to all fields, including individual flags within the status register and individual memory addresses.

### 4.4 RAM (nibble trie)

See section 3.6. Four levels, 16-way branching, ~512 bytes allocated per write, O(changed bytes) diff via pointer equality.

### 4.5 Execution trace

A lazy `[CPUState]` produced by `scanl`/iterate. Each state shares RAM with its predecessor via the persistent trie. GHC's garbage collector reclaims old states once no live reference points to them — a sliding observation window consumes constant memory.

---

## 5. Implementation Plan

### Phase 1: Foundation (assembler-only, no emulator)

Build the assembler in this order. Each step produces a usable tool.

| Step | Module(s)     | Deliverable                                          |
|------|---------------|------------------------------------------------------|
| 1.1  | ISA           | Opcode/AddrMode types, encode/decode, instrSize      |
| 1.2  | Asm (core)    | ASM monad with MonadFix, emit, here                  |
| 1.3  | Asm (instrs)  | All 56 opcodes with raw addressing mode constructors |
| 1.4  | Syntax        | `#`, tuples, `!`, typed variables, AutoAddr instances |
| 1.5  | Asm (alloc)   | Zero-page allocator (allocZP, allocZP16, allocPtr)   |
| 1.6  | C64 (data)    | byte, word, petscii, incbin, align                   |
| 1.7  | C64 (output)  | basicStub, writePRG, exportViceLabels                |
| 1.8  | Macro (flow)  | if_, while_, for_x                                   |
| 1.9  | Macro (16bit) | add16, sub16, inc16, cmp16, mov16, store16           |

**Milestone: working C64 assembler.** Can write, assemble, and run programs on real hardware or VICE.

### Phase 2: Emulator and testing

| Step | Module(s)     | Deliverable                                          |
|------|---------------|------------------------------------------------------|
| 2.1  | RAM           | Nibble trie: empty, readByte, writeByte, fromLazyList |
| 2.2  | Emu (core)    | CPUState, step function for all instructions          |
| 2.3  | Emu (fetch)   | Fetch-decode-execute loop from RAM                    |
| 2.4  | Test (gen)    | TestInstr type, genInstr, genSnippet                  |
| 2.5  | Test (diff)   | prop_asmMatchesEmu (core differential property)       |
| 2.6  | Test (macro)  | prop_add16_correct and properties for all macros      |
| 2.7  | RAM (diff)    | diffRAM with pointer equality                         |

**Milestone: differential testing operational.** Every instruction and macro is verified by QuickCheck.

### Phase 3: Lazy traces and observation

| Step | Module(s)     | Deliverable                                          |
|------|---------------|------------------------------------------------------|
| 3.1  | Trace         | Lazy trace, watchReg, watch16                         |
| 3.2  | Trace (lens)  | memAt lens, flag lens, composed observation           |
| 3.3  | Trace (diff)  | deltas using diffRAM                                  |
| 3.4  | RAM (lazy)    | Verify lazy initialization from assembler byte stream |

**Milestone: lazy demand-driven emulation.** Observation through lenses forces only the computation needed to answer the question.

### Phase 4: High-level semantics and Futamura

| Step | Module(s)     | Deliverable                                          |
|------|---------------|------------------------------------------------------|
| 4.1  | Semantics     | Semantics typeclass, Val type family                  |
| 4.2  | Semantics     | Concrete interpreter instance (Val = Word8)           |
| 4.3  | Semantics     | Code generator instance (Val = Expr, partial eval)    |
| 4.4  | Semantics     | Reified free monad instance (second projection)       |

**Milestone: first Futamura projection demonstrated.** A high-level program interpreted through the code generator produces optimized 6502 assembly.

---

## 6. Migration from Previous Assembler-Only Design

The previous design (see "Embedded 6502 Assembler DSL — Implementation Plan") defined all instructions directly on the `ASM` monad. The new design abstracts instructions behind typeclasses. The migration is incremental and non-breaking.

### Step M1: Extract ISA module

Move the `Instruction`, `Opcode`, `AddrMode` types and encoding tables out of the assembler into a standalone `ISA` module. No behavioral change. All existing code continues to work.

### Step M2: Introduce typeclasses alongside existing functions

Define the tagless-final typeclasses in `Class.hs`. Implement them for the `Asm` monad by delegating to the existing instruction functions. The old functions are still available; the typeclasses add a second way to call them.

```haskell
-- Old: lda_imm :: Word8 -> ASM ()  (monomorphic)
-- New: lda :: Loadable m Imm => Imm -> m ()  (polymorphic)
-- Bridge: instance Loadable Asm Imm where lda (Imm v) = lda_imm v
```

### Step M3: Move syntax into Syntax module

Extract the `#`, tuple, and `!` instances into `Syntax.hs`. These currently live in or near the assembler; they should be independent of the interpreter.

### Step M4: Build emulator implementing the same typeclasses

Create `Emu.hs` implementing the Layer 1 typeclasses. This is new code, not a refactor. Existing assembler code is unaffected.

### Step M5: Build RAM module

Create the nibble trie as a standalone module. Wire it into `Emu.hs` as the RAM implementation. No impact on the assembler.

### Step M6: Migrate macros to polymorphic signatures

Change macros from `add16 :: Var16 -> Var16 -> Var16 -> ASM ()` to `add16 :: (Loadable m Var8, ...) => Var16 -> Var16 -> Var16 -> m ()`. Existing code that calls `add16` in the `Asm` monad continues to work unchanged — the polymorphic type is strictly more general.

### Step M7: Add differential tests

With both interpreters and polymorphic macros in place, add the QuickCheck test suite. This is purely additive.

### Step M8: Deprecate monomorphic instruction functions

Once all call sites use the polymorphic typeclass methods, the old monomorphic functions (`lda_imm`, `lda_zp`, etc.) can be deprecated and eventually removed. This is optional — they can coexist indefinitely.

### Migration invariant

At every step, all existing assembler programs compile and produce identical .prg output. The migration adds capabilities (emulation, testing, polymorphism) without removing or changing any existing behavior.

---

## 7. Required GHC Extensions

```haskell
{-# LANGUAGE RecursiveDo #-}                -- mdo blocks for MonadFix labels
{-# LANGUAGE MultiParamTypeClasses #-}      -- Loadable m op, Indirectable a ix r
{-# LANGUAGE FunctionalDependencies #-}     -- | a ix -> result
{-# LANGUAGE FlexibleInstances #-}          -- instances on tuples, Word8, Word16
{-# LANGUAGE FlexibleContexts #-}           -- polymorphic macro constraints
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- deriving Num on newtypes
{-# LANGUAGE TypeFamilies #-}               -- Val type family in Semantics
{-# LANGUAGE TemplateHaskell #-}            -- makeLenses for CPUState
{-# LANGUAGE RankNTypes #-}                 -- Traversal' in ISA
{-# LANGUAGE ScopedTypeVariables #-}        -- type annotations in local bindings
{-# LANGUAGE DataKinds #-}                  -- Phase kind (if type-level staging)
{-# LANGUAGE GADTs #-}                      -- Expr GADT in Semantics
```

---

## 8. Example: Complete Program Under New Design

```haskell
{-# LANGUAGE RecursiveDo #-}
import Prelude hiding ((!))
import MOS6502

main :: IO ()
main = do
  let (bytes, labels) = runAsm c64Default $ mdo

        basicStub entry

        entry <- here

        -- Clear screen
        lda # 0x20
        for_x 0x00 $ do
          sta (0x0400, X)
          sta (0x0500, X)
          sta (0x0600, X)
          sta (0x0700, X)

        -- Set up source pointer
        lda # lo msgAddr
        sta src
        lda # hi msgAddr
        sta (ptrHi src)         -- helper: access high byte of Ptr

        -- Print loop using indirect-Y addressing
        ldy # 0x00
        loop <- here
        lda (src ! Y)
        beq done
        sta (0x0400, Y)
        iny
        bne loop

        done <- here
        rts

        -- Data section
        msgAddr <- here
        petscii "HELLO FROM HASKELL"
        byte [0x00]

        -- Sine table (computed at assembly time by Haskell)
        align 256
        sineTable <- here
        byte [ round (128 + 127 * sin (2 * pi * i / 256))
             | i <- [0..255] ]

        -- Variable allocation (zero page)
        src <- allocPtr

  writePRG "hello.prg" 0x0801 bytes
  exportViceLabels "hello.vs" labels

  -- Also: run it in the emulator and verify
  let initState = loadProgram 0x0801 bytes defaultCPUState
      states = trace initState
      screenAfter = take 18 [ s ^. ram . memAt (0x0400 + i)
                             | let s = runUntil (\s -> s ^. regPC == addrOf done) initState
                             , i <- [0..17] ]
  putStrLn $ "Screen: " ++ map fromPetscii screenAfter
  -- Output: "Screen: HELLO FROM HASKELL"
```

---

## 9. Design Rationale Summary

| Decision | Rationale |
|----------|-----------|
| Tagless-final over free monad | Better performance, no intermediate AST allocation, extensible with new instances |
| MonadFix over two-pass assembly | Single-pass, composable, forward refs are just laziness |
| Nibble trie over IntMap/Array | Fixed depth (4), predictable performance, 512B/write sharing, lazy leaves |
| Resolve-then-interpret over mixed phases | Clean separation, emulator sees only concrete instructions, no laziness hazards in execution |
| Polymorphic macros over assembler-specific | Same code tests via both interpreters, enables Futamura projection path |
| Lenses over direct field access | Composable observation of nested state, formal connection to derivatives/one-hole contexts |
| Lazy trace over snapshot-based | Demand-driven, GC-friendly, composes with lenses for minimal forcing |
| Operators (#, !, comma) over constructors | Reads like real assembly, type-checked, no parsing overhead |


---

## 10. Outlook

User asks:

What are new use cases and unique possibilities enabled by our lazy design? Could we also build MonteCarlo Tree search in program space on top of this in the future to search for programs that fulfill desired properties?

Claude:

These are really exciting questions. The lazy architecture opens up capabilities that would be impractical or impossible in a strict design, and yes, MCTS over program space is not only feasible but naturally fits the architecture. Let me work through both.

### Unique capabilities of the lazy design

#### Infinite program spaces with finite observation

Because the trace is lazy, you can define *infinite* or *parametric* programs and observe only what you need:

```haskell
-- An infinite program: counts forever
counter :: Asm ()
counter = mdo
  loop <- here
  inc playerX
  jmp loop

-- But we only observe the first 256 steps
take 256 (watchReg regA (trace (load counter initState)))
```

This extends to something more powerful — you can define a *family* of programs parameterized by values, and lazily explore the parameter space without assembling every variant:

```haskell
-- A family of programs parameterized by an unrolling factor
copyRoutine :: Int -> Asm ()
copyRoutine n = mdo
  ldy # 0x00
  loop <- here
  replicateM_ n $ do    -- unroll n times
    lda (src ! Y)
    sta (dst ! Y)
    iny
  bne loop

-- Compare cycle counts across unrolling factors WITHOUT
-- fully assembling or executing any of them until needed
cycleCounts :: [(Int, Int)]
cycleCounts =
  [ (n, runUntilRTS (load (copyRoutine n) s0) ^. cycles)
  | n <- [1..8]
  ]

-- Laziness means: if you only inspect `head cycleCounts`,
-- only the n=1 variant is assembled and executed.
```

#### Backwards reasoning from observations

Because lenses and laziness compose, you can ask questions *about* the execution that would normally require running the whole thing, but lazily only compute what's needed:

```haskell
-- "What's the minimum number of steps until address $D020
-- (the border color register) gets written?"
firstBorderWrite :: CPUState -> Maybe Int
firstBorderWrite s0 =
  findIndex (elem 0xD020 . map fst3) (deltas (trace s0))
  where fst3 (a, _, _) = a
```

This scans the trace lazily. Each step, only the RAM diff is computed (which is O(writes per instruction) via pointer equality), and the scan stops as soon as a write to `$D020` is found. If the program never writes to the border, this diverges — but that's correct, it's an infinite search over an infinite trace.

More interestingly, you can observe *properties of the assembled code* without executing it:

```haskell
-- "How many bytes does this routine take if I inline vs not?"
inlineCost = length (runAsmBytes (inlinedVersion routine))
callCost   = length (runAsmBytes (calledVersion routine))

-- Neither version is EXECUTED — only assembled, and only enough
-- to count bytes. The lazy byte stream doesn't need to resolve
-- branch targets if we're only measuring size.
```

#### Speculative assembly with lazy pruning

You can assemble multiple candidate implementations and lazily evaluate only the one that turns out best:

```haskell
-- Generate several multiplication strategies, pick the fastest
bestMultiply :: Word8 -> Word8 -> Asm ()
bestMultiply a b =
  let candidates =
        [ ("shift-add",   shiftAddMultiply a b)
        , ("table",       tableMultiply a b)
        , ("unrolled",    unrolledMultiply a b)
        ]
      costs = [ (name, estimateCycles impl) | (name, impl) <- candidates ]
      (bestName, _) = minimumBy (comparing snd) costs
  in  fromJust (lookup bestName candidates)
```

If `estimateCycles` only examines instruction sizes and the cost model (not actual execution), most of the assembly work is never done. Only the winning candidate gets fully assembled.

#### Time-travel debugging

The persistent trie enables something that mutable emulators can't do efficiently — moving *backward* in time:

```haskell
-- Find where a bug manifests, then examine earlier states
buggyState = states !! 5000         -- something wrong here
-- Look at what happened 10 steps before:
prelude = states !! 4990            -- still in memory, shared trie!

-- Diff the two:
diffRAM (prelude ^. ram) (buggyState ^. ram)
-- Shows exactly which memory locations changed in those 10 steps
```

With mutable arrays, state 4990 would be gone — you'd have to re-execute from the beginning. With persistent tries, all intermediate states coexist and share structure. The GC only collects states that no one is looking at.

#### Bisecting over the trace

Combined with time-travel, you can do binary search over the execution history:

```haskell
-- "At what step does the screen first show a wrong character?"
bisectTrace :: (CPUState -> Bool) -> [CPUState] -> Int
bisectTrace prop states = go 0 (length states - 1)
  where
    go lo hi
      | lo >= hi  = lo
      | prop (states !! mid) = go lo mid
      | otherwise = go (mid + 1) hi
      where mid = (lo + hi) `div` 2
```

Each `states !! n` forces execution up to step n, but because the trace is lazy, earlier probes don't redo work — GHC memoizes the list spine. And the persistent RAM means every probed state remains valid and comparable.

#### Cross-program RAM diff

Because RAM is a pure value, you can compare the memory effects of *different programs*:

```haskell
-- "Does my optimized version produce the same screen output?"
original  = runUntilRTS (load origProgram s0)
optimized = runUntilRTS (load optProgram s0)

screenDiff = diffRAM (original ^. ram) (optimized ^. ram)
  & filter (\(addr, _, _) -> addr >= 0x0400 && addr <= 0x07FF)

-- If screenDiff is empty, both programs produce identical screen content.
-- The diff is O(changed bytes), not O(64KB).
```

This is a very powerful optimization validation tool — you can prove that a code transformation preserves observable behavior for a specific input without formal verification.

#### Lazy superoptimization probing

You can test whether a candidate instruction sequence is *worth fully evaluating* by checking cheap properties first:

```haskell
-- Does this candidate produce the right output size?
-- (Check without executing)
candidateSize = instrSize candidate
if candidateSize > maxAllowedSize
  then reject           -- never assemble or execute
  else do
    -- Does it produce the right result for one test input?
    let s1 = execSnippet candidate testInput1
    if s1 ^. regA /= expectedA
      then reject       -- executed but only one test case
      else do
        -- Does it pass all test cases?
        let results = map (execSnippet candidate) allInputs
        ...
```

Each level of the funnel is lazier than the next. Size checking forces no execution. One-input testing forces one execution. Full testing forces all executions. Candidates that fail early never trigger later (expensive) checks. The lazy architecture makes this natural rather than requiring explicit short-circuit control flow.

---

### Monte Carlo Tree Search over program space

Yes, this is feasible and the architecture supports it remarkably well. Let me design how it would work.

#### The search space

A 6502 program is a sequence of instructions. The tree structure is:

```
Root
├── LDA #$00
│   ├── STA $FB
│   │   ├── INX → ...
│   │   ├── DEX → ...
│   │   └── RTS → (terminal)
│   ├── ADC #$01
│   │   └── ...
│   └── TAX
│       └── ...
├── LDX #$00
│   └── ...
├── CLC
│   └── ...
└── ...
```

Each node is a partial program. Children extend it by one instruction. Leaves are complete programs (ended by RTS or reaching a length limit). The branching factor is roughly 200–300 (number of valid opcode/operand combinations), but can be reduced dramatically with domain knowledge.

#### The evaluation function

This is where the lazy architecture provides its key advantage. Given a candidate program (a path through the tree), you need to evaluate how "good" it is against a specification. The specification is a set of input/output pairs or properties:

```haskell
data Spec = Spec
  { testCases   :: [(CPUState, CPUState -> Bool)]
    -- (initial state, predicate on final state)
  , maxSize     :: Int         -- byte count limit
  , maxCycles   :: Int         -- cycle count limit
  }

evaluate :: Spec -> [Instruction] -> Double
evaluate spec instrs =
  let size = sum (map instrSize instrs)
      sizeOk = size <= maxSize spec
  in if not sizeOk then 0.0       -- prune immediately, no execution
     else
       let results =
             [ (passes, cyc)
             | (initState, pred) <- testCases spec
             , let finalState = execSnippet instrs initState
             , let passes = pred finalState
             , let cyc = finalState ^. cycles
             ]
           passRate = fromIntegral (length (filter fst results))
                    / fromIntegral (length results)
           avgCycles = avg (map snd results)
           cycleScore = if avgCycles <= maxCycles spec
                        then 1.0
                        else fromIntegral (maxCycles spec) / avgCycles
       in passRate * 0.8 + cycleScore * 0.2
```

The laziness payoff: `execSnippet` runs the emulator on the persistent trie. Each test case gets its own execution with shared initial RAM. If the first test case fails (passRate is already low), the MCTS can decide this branch isn't worth exploring further — and the remaining test cases are never executed because `results` is lazy.

#### The MCTS algorithm adapted for program synthesis

```haskell
data MCTSNode = MCTSNode
  { program     :: [Instruction]     -- partial program so far
  , visits      :: !Int
  , totalReward :: !Double
  , children    :: Map Instruction MCTSNode  -- lazily expanded!
  }

-- The four MCTS phases:

-- 1. SELECTION: walk down the tree using UCB1
select :: MCTSNode -> [Instruction] -> (MCTSNode, [Instruction])
select node path
  | Map.null (children node) = (node, path)  -- leaf: expand
  | otherwise =
      let best = maximumBy (comparing ucb1) (Map.toList (children node))
      in  select (snd best) (path ++ [fst best])

ucb1 :: (Instruction, MCTSNode) -> Double
ucb1 (_, child) =
  (totalReward child / fromIntegral (visits child))
  + sqrt (2 * log (fromIntegral parentVisits) / fromIntegral (visits child))

-- 2. EXPANSION: add a new child (one new instruction)
expand :: MCTSNode -> Gen Instruction -> MCTSNode
expand node newInstr =
  let child = MCTSNode
        { program = program node ++ [newInstr]
        , visits = 0
        , totalReward = 0
        , children = Map.empty
        }
  in node { children = Map.insert newInstr child (children node) }

-- 3. SIMULATION: random rollout to completion
simulate :: [Instruction] -> Spec -> Gen Double
simulate partial spec = do
  -- Random completion up to max length
  remaining <- replicateM (maxLen - length partial) genRandomInstr
  let full = partial ++ remaining ++ [rtsInstr]
  pure (evaluate spec full)

-- 4. BACKPROPAGATION: update visit counts and rewards up the path
backpropagate :: Double -> [MCTSNode] -> [MCTSNode]
```

#### Why lazy evaluation makes MCTS over programs practical

**Lazy evaluation of candidates.** Each simulation involves assembling and executing a candidate program. With the lazy architecture, assembly only resolves what execution demands. A candidate that crashes early (illegal memory access, infinite loop detected by cycle limit) terminates immediately without assembling the rest.

**Shared initial state.** All candidates run against the same test cases with the same initial RAM. The persistent trie means each candidate's execution shares the initial trie structure. There's no per-candidate cost for setting up 64KB of memory.

**Incremental evaluation.** When MCTS expands a node (adding one instruction to a partial program), the evaluation of the longer program can potentially *reuse* work from evaluating the shorter prefix. With the lazy trace, if you've already computed the first 5 steps for the parent node's program, adding a 6th instruction only needs to compute step 6 — steps 1–5 are memoized in the lazy list.

```haskell
-- Incremental trace: extend a parent's trace with one more instruction
extendTrace :: [CPUState]   -- parent's trace (already computed)
            -> Instruction   -- new instruction to append
            -> [CPUState]    -- extended trace (shares parent's prefix)
extendTrace parentTrace newInstr =
  let lastState = last parentTrace  -- O(1) if we keep a pointer to the end
      nextState = execSingle newInstr lastState
  in  parentTrace ++ [nextState]    -- shares the entire prefix
```

Actually, a more efficient representation for MCTS would store the *last state* at each node rather than the full trace:

```haskell
data MCTSNode = MCTSNode
  { partialProgram :: [Instruction]
  , stateAfter     :: Map InputId CPUState
      -- CPU state after executing this partial program on each test input
      -- Persistent trie means these are cheap to keep around
  , visits         :: !Int
  , totalReward    :: !Double
  , children       :: Map Instruction MCTSNode
  }
```

When expanding a child, you take each parent's `stateAfter`, execute one more instruction, and get the child's `stateAfter`. The persistent RAM trie means each of these stored states is small (they share most of their memory with the parent's states and with the initial state).

#### Smart narrowing of the search space

The raw branching factor (~250 valid instructions) is too large for naive MCTS. But you can prune it intelligently using information from the emulator:

```haskell
-- Only generate instructions that are "relevant" given current state
relevantNextInstructions :: CPUState -> [Instruction]
relevantNextInstructions s =
  let aVal = s ^. regA
      xVal = s ^. regX
      flags = s ^. regP
      usedZP = ... -- which ZP addresses have been touched
  in  filter (isRelevant s) allInstructions

isRelevant :: CPUState -> Instruction -> Bool
isRelevant s (Instruction STA (ZeroPage addr))
  = addr `elem` allocatedVars             -- don't store to random addresses
isRelevant s (Instruction BEQ _)
  = True                                   -- branches are always interesting
isRelevant s (Instruction ADC (Immediate _))
  = s ^. flag Carry == False              -- ADC without CLC first is suspect
  || ...                                   -- but might be intentional
isRelevant _ _ = True                      -- default: allow
```

This domain-knowledge pruning can reduce the effective branching factor to 20–50, making MCTS tractable for short programs (5–15 instructions).

#### Guided simulation with learned heuristics

Instead of pure random rollouts, you can use a learned policy to guide simulations toward more promising completions:

```haskell
-- A simple policy: probability distribution over next instructions
-- conditioned on current CPU state
type Policy = CPUState -> [(Instruction, Double)]

-- Train this from a corpus of known-good 6502 code:
-- "After LDA #imm, STA zp is much more likely than DEX"
-- This dramatically improves rollout quality.

simulateGuided :: Policy -> [Instruction] -> Spec -> Gen Double
simulateGuided policy partial spec = do
  let s0 = execPartial partial initState
  remaining <- rolloutWithPolicy policy s0 (maxLen - length partial)
  let full = partial ++ remaining ++ [rtsInstr]
  pure (evaluate spec full)
```

#### Concrete use cases for MCTS program synthesis

**Superoptimization.** Given a reference implementation (e.g., your `add16` macro), find a shorter or faster instruction sequence that produces the same results. The spec is derived from the reference:

```haskell
superoptSpec :: [Instruction] -> Spec
superoptSpec reference = Spec
  { testCases = [ (randomState seed, \s ->
        execSnippet [s] reference ^. ram == s ^. ram
        && execSnippet [s] reference ^. regA == s ^. regA)
      | seed <- [1..100] ]
  , maxSize = instrSize reference - 1   -- must be strictly shorter
  , maxCycles = totalCycles reference    -- must be no slower
  }
```

**Peephole pattern discovery.** Run MCTS to find equivalent short sequences for common instruction patterns. Any time MCTS finds a shorter equivalent, that becomes a new peephole rule for the optimizer:

```haskell
-- MCTS found that these 3 instructions:
--   LDA $FB / CLC / ADC #$01
-- can be replaced by:
--   INC $FB / LDA $FB
-- when the carry flag state doesn't matter afterward.
-- This becomes a peephole rule automatically.
```

**Synthesis from examples.** Given input/output pairs, synthesize a program that transforms input to output:

```haskell
-- "Find a program that sorts 4 bytes in zero page $FB-$FE"
sortSpec :: Spec
sortSpec = Spec
  { testCases =
      [ (stateWith [0xFB..0xFE] [3,1,4,2],
         \s -> readZP s [0xFB..0xFE] == [1,2,3,4])
      , (stateWith [0xFB..0xFE] [9,7,8,6],
         \s -> readZP s [0xFB..0xFE] == [6,7,8,9])
      -- ... more test cases
      ]
  , maxSize = 50
  , maxCycles = 500
  }
```

**Invariant discovery.** Run many random programs, observe which properties hold across all of them, discover architectural invariants you didn't know about:

```haskell
-- "What's always true about register A after a sequence of
-- ADC/SBC instructions starting from A=0?"
discoverInvariants :: Gen [Instruction] -> (CPUState -> [String]) -> IO [String]
discoverInvariants gen extractProps = do
  programs <- generate (vectorOf 10000 gen)
  let allProps = map (\p ->
        let s = execSnippet p (initState & regA .~ 0)
        in  extractProps s
        ) programs
  -- Properties that hold for ALL 10000 programs are likely invariants
  pure (foldl1 intersect allProps)
```

#### Beyond MCTS: genetic programming with lazy fitness

The architecture also supports genetic programming naturally. Crossover is splicing instruction sequences. Mutation is replacing/inserting/deleting instructions. Fitness evaluation uses the same lazy evaluation pipeline:

```haskell
data Individual = Individual
  { genome  :: [Instruction]
  , fitness :: Double          -- lazily evaluated!
  }

-- Crossover: take prefix from parent A, suffix from parent B
crossover :: Individual -> Individual -> Int -> Individual
crossover a b point = Individual
  { genome = take point (genome a) ++ drop point (genome b)
  , fitness = undefined  -- lazy! computed only when selected/compared
  }

-- Tournament selection forces fitness evaluation only for
-- the individuals actually compared
tournament :: [Individual] -> Gen Individual
tournament pop = do
  contestants <- replicateM 4 (elements pop)
  pure (maximumBy (comparing fitness) contestants)
  -- Only these 4 individuals have their fitness forced.
  -- The other N-4 individuals in the population remain unevaluated.
```

With a population of 1000 and tournament size 4, each generation forces fitness evaluation for roughly `4 × (1000/4) = 1000` individuals — but if many tournaments pick the same individual, its cached fitness is reused. With lazy evaluation, the exact number of evaluations adapts to what's actually needed.

#### The deeper connection: programs as lazy data

The most profound consequence of the lazy design is that *programs become data that you manipulate without executing*. You can build a program, measure its size, inspect its structure, compare it to another program, splice parts together, and transform it — all without ever running the emulator. Execution is one possible *observation* of a program, not an inherent property.

This is what makes search over program space tractable. In a strict system, constructing a candidate program also executes it (or at least forces all its assembly). In a lazy system, constructing a candidate is cheap — it's just building a thunk. Only the evaluation function (which the search algorithm invokes selectively) forces actual computation. The search can explore millions of candidate program *structures* while only evaluating thousands of them, and only fully executing hundreds.

The persistent trie amplifies this: states from different candidate evaluations that happen to share memory contents share the actual trie nodes. If 100 candidates all start from the same initial state and diverge after 10 instructions, the first 10 states in each trace share RAM completely. The total memory is proportional to the *differences* between candidates, not their total state.

This is, in a sense, the ultimate expression of the derivative/one-hole-context idea: each candidate program is a small perturbation (a filled hole) in a shared structure, and laziness ensures that only the perturbation — not the shared context — costs computation.
