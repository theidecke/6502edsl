{-# LANGUAGE FunctionalDependencies #-}

module Asm.Mos6502
    ( -- * Operand newtypes
      Imm(..), ZP(..), ZPX(..), ZPY(..)
    , Abs(..), AbsX(..), AbsY(..)
    , Ind(..), IndX(..), IndY(..)
      -- * Operand typeclass
    , Operand(..)
      -- * Addressing mode tags
    , Mode(..)
      -- * Opcode table lookup
    , opcodeTable
    , opcodeFor
      -- * Singleton types
    , X_(..), Y_(..), A_(..)
      -- * Variable types
    , Var8(..), Var16(..), Ptr(..)
    , lo16, hi16
      -- * Addressing mode sugar
    , (#), Indirectable(..)
      -- * Typed allocation
    , allocVar8, allocVar16, allocPtr
      -- * Operand instructions
    , lda, ldx, ldy, sta, stx, sty
    , adc, sbc, and_, ora, eor
    , cmp, cpx, cpy
    , asl, lsr, rol, ror
    , inc, dec, bit
      -- * Implied instructions
    , brk, clc, cld, cli, clv
    , dex, dey, inx, iny, nop
    , pha, php, pla, plp
    , rti, rts
    , sec, sed, sei
    , tax, tay, tsx, txa, txs, tya
      -- * Accumulator instructions
    , asl_a, lsr_a, rol_a, ror_a
      -- * Branch instructions
    , bcc, bcs, beq, bmi, bne, bpl, bvc, bvs
      -- * Jump instructions
    , jmp, jmp_ind, jmpInd, jsr
    ) where

import Data.Map.Strict qualified as Map
import Data.Word (Word8, Word16)
import GHC.Stack (HasCallStack)
import Numeric (showHex)

import Asm.Monad (ASM, emit, label, lo, hi, allocZP)
import ISA.Mos6502 (Opcode(..))

-- ---------------------------------------------------------------------------
-- Addressing mode tags (no payload — just for opcode table lookup)
-- ---------------------------------------------------------------------------

data Mode
    = MImplied | MAccumulator | MImmediate
    | MZeroPage | MZeroPageX | MZeroPageY
    | MAbsolute | MAbsoluteX | MAbsoluteY
    | MIndirect | MIndirectX | MIndirectY
    | MRelative
    deriving (Show, Eq, Ord)

-- ---------------------------------------------------------------------------
-- Operand newtypes
-- ---------------------------------------------------------------------------

newtype Imm  = Imm  Word8  deriving (Show)
newtype ZP   = ZP   Word8  deriving (Show)
newtype ZPX  = ZPX  Word8  deriving (Show)
newtype ZPY  = ZPY  Word8  deriving (Show)
newtype Abs  = Abs  Word16 deriving (Show)
newtype AbsX = AbsX Word16 deriving (Show)
newtype AbsY = AbsY Word16 deriving (Show)
newtype Ind  = Ind  Word16 deriving (Show)
newtype IndX = IndX Word8  deriving (Show)
newtype IndY = IndY Word8  deriving (Show)

-- ---------------------------------------------------------------------------
-- Operand typeclass
-- ---------------------------------------------------------------------------

class Operand a where
    operandMode  :: a -> Mode
    operandBytes :: a -> [Word8]

instance Operand Imm  where operandMode _ = MImmediate;  operandBytes (Imm  b) = [b]
instance Operand ZP   where operandMode _ = MZeroPage;   operandBytes (ZP   b) = [b]
instance Operand ZPX  where operandMode _ = MZeroPageX;  operandBytes (ZPX  b) = [b]
instance Operand ZPY  where operandMode _ = MZeroPageY;  operandBytes (ZPY  b) = [b]
instance Operand Abs  where operandMode _ = MAbsolute;   operandBytes (Abs  w) = [lo w, hi w]
instance Operand AbsX where operandMode _ = MAbsoluteX;  operandBytes (AbsX w) = [lo w, hi w]
instance Operand AbsY where operandMode _ = MAbsoluteY;  operandBytes (AbsY w) = [lo w, hi w]
instance Operand Ind  where operandMode _ = MIndirect;   operandBytes (Ind  w) = [lo w, hi w]
instance Operand IndX where operandMode _ = MIndirectX;  operandBytes (IndX b) = [b]
instance Operand IndY where operandMode _ = MIndirectY;  operandBytes (IndY b) = [b]

-- ---------------------------------------------------------------------------
-- Singleton types for register indices
-- ---------------------------------------------------------------------------

data X_ = X
data Y_ = Y
data A_ = A

-- ---------------------------------------------------------------------------
-- Typed variable newtypes
-- ---------------------------------------------------------------------------

newtype Var8  = Var8  Word8  deriving (Show, Eq)
newtype Var16 = Var16 Word8  deriving (Show, Eq)
data    Ptr   = Ptr   Word8  deriving (Show, Eq)

instance Num Var8 where
    Var8 a + Var8 b = Var8 (a + b)
    fromInteger = Var8 . fromInteger
    Var8 a * Var8 b = Var8 (a * b)
    abs    = id
    signum (Var8 a) = Var8 (signum a)
    negate (Var8 a) = Var8 (negate a)

instance Num Ptr where
    Ptr a + Ptr b = Ptr (a + b)
    fromInteger = Ptr . fromInteger
    Ptr a * Ptr b = Ptr (a * b)
    abs    = id
    signum (Ptr a) = Ptr (signum a)
    negate (Ptr a) = Ptr (negate a)

lo16 :: Var16 -> Var8
lo16 (Var16 a) = Var8 a

hi16 :: Var16 -> Var8
hi16 (Var16 a) = Var8 (a + 1)

-- ---------------------------------------------------------------------------
-- Operand instances for bare types and tuples
-- ---------------------------------------------------------------------------

instance Operand Word8  where operandMode _ = MZeroPage;    operandBytes w = [w]
instance Operand Word16 where operandMode _ = MAbsolute;    operandBytes w = [lo w, hi w]
instance Operand A_     where operandMode _ = MAccumulator; operandBytes _ = []
instance Operand Var8   where operandMode _ = MZeroPage;    operandBytes (Var8 a) = [a]
instance Operand Ptr    where operandMode _ = MZeroPage;    operandBytes (Ptr a) = [a]

instance Operand (Word8,  X_) where operandMode _ = MZeroPageX;  operandBytes (w, _) = [w]
instance Operand (Word8,  Y_) where operandMode _ = MZeroPageY;  operandBytes (w, _) = [w]
instance Operand (Word16, X_) where operandMode _ = MAbsoluteX;  operandBytes (w, _) = [lo w, hi w]
instance Operand (Word16, Y_) where operandMode _ = MAbsoluteY;  operandBytes (w, _) = [lo w, hi w]
instance Operand (Var8,   X_) where operandMode _ = MZeroPageX;  operandBytes (Var8 a, _) = [a]
instance Operand (Var8,   Y_) where operandMode _ = MZeroPageY;  operandBytes (Var8 a, _) = [a]
instance Operand (Ptr,    X_) where operandMode _ = MZeroPageX;  operandBytes (Ptr a, _) = [a]
instance Operand (Ptr,    Y_) where operandMode _ = MZeroPageY;  operandBytes (Ptr a, _) = [a]

-- ---------------------------------------------------------------------------
-- (#) operator — immediate mode sugar
-- ---------------------------------------------------------------------------

infixl 8 #

(#) :: (Imm -> ASM ()) -> Word8 -> ASM ()
f # v = f (Imm v)

-- ---------------------------------------------------------------------------
-- (!) operator — indirect mode sugar
-- ---------------------------------------------------------------------------

infixl 8 !

class Indirectable a ix result | a ix -> result where
    (!) :: a -> ix -> result

instance Indirectable Word8 Y_ IndY where (!) a Y = IndY a
instance Indirectable Word8 X_ IndX where (!) a X = IndX a
instance Indirectable Ptr   Y_ IndY where (!) (Ptr a) Y = IndY a
instance Indirectable Ptr   X_ IndX where (!) (Ptr a) X = IndX a

-- ---------------------------------------------------------------------------
-- Typed allocation
-- ---------------------------------------------------------------------------

allocVar8 :: ASM Var8
allocVar8 = Var8 <$> allocZP 1

allocVar16 :: ASM Var16
allocVar16 = Var16 <$> allocZP 2

allocPtr :: ASM Ptr
allocPtr = Ptr <$> allocZP 2

-- ---------------------------------------------------------------------------
-- Opcode table
-- ---------------------------------------------------------------------------

opcodeTable :: Map.Map (Opcode, Mode) Word8
opcodeTable = Map.fromList
    -- ADC
    [ ((ADC, MImmediate), 0x69), ((ADC, MZeroPage), 0x65), ((ADC, MZeroPageX), 0x75)
    , ((ADC, MAbsolute),  0x6D), ((ADC, MAbsoluteX), 0x7D), ((ADC, MAbsoluteY), 0x79)
    , ((ADC, MIndirectX), 0x61), ((ADC, MIndirectY), 0x71)
    -- AND
    , ((AND, MImmediate), 0x29), ((AND, MZeroPage), 0x25), ((AND, MZeroPageX), 0x35)
    , ((AND, MAbsolute),  0x2D), ((AND, MAbsoluteX), 0x3D), ((AND, MAbsoluteY), 0x39)
    , ((AND, MIndirectX), 0x21), ((AND, MIndirectY), 0x31)
    -- ASL
    , ((ASL, MAccumulator), 0x0A)
    , ((ASL, MZeroPage), 0x06), ((ASL, MZeroPageX), 0x16)
    , ((ASL, MAbsolute), 0x0E), ((ASL, MAbsoluteX), 0x1E)
    -- BCC..BVS (branches)
    , ((BCC, MRelative), 0x90), ((BCS, MRelative), 0xB0)
    , ((BEQ, MRelative), 0xF0), ((BMI, MRelative), 0x30)
    , ((BNE, MRelative), 0xD0), ((BPL, MRelative), 0x10)
    , ((BVC, MRelative), 0x50), ((BVS, MRelative), 0x70)
    -- BIT
    , ((BIT, MZeroPage), 0x24), ((BIT, MAbsolute), 0x2C)
    -- BRK
    , ((BRK, MImplied), 0x00)
    -- CLC, CLD, CLI, CLV
    , ((CLC, MImplied), 0x18), ((CLD, MImplied), 0xD8)
    , ((CLI, MImplied), 0x58), ((CLV, MImplied), 0xB8)
    -- CMP
    , ((CMP, MImmediate), 0xC9), ((CMP, MZeroPage), 0xC5), ((CMP, MZeroPageX), 0xD5)
    , ((CMP, MAbsolute),  0xCD), ((CMP, MAbsoluteX), 0xDD), ((CMP, MAbsoluteY), 0xD9)
    , ((CMP, MIndirectX), 0xC1), ((CMP, MIndirectY), 0xD1)
    -- CPX
    , ((CPX, MImmediate), 0xE0), ((CPX, MZeroPage), 0xE4), ((CPX, MAbsolute), 0xEC)
    -- CPY
    , ((CPY, MImmediate), 0xC0), ((CPY, MZeroPage), 0xC4), ((CPY, MAbsolute), 0xCC)
    -- DEC
    , ((DEC, MZeroPage), 0xC6), ((DEC, MZeroPageX), 0xD6)
    , ((DEC, MAbsolute), 0xCE), ((DEC, MAbsoluteX), 0xDE)
    -- DEX, DEY
    , ((DEX, MImplied), 0xCA), ((DEY, MImplied), 0x88)
    -- EOR
    , ((EOR, MImmediate), 0x49), ((EOR, MZeroPage), 0x45), ((EOR, MZeroPageX), 0x55)
    , ((EOR, MAbsolute),  0x4D), ((EOR, MAbsoluteX), 0x5D), ((EOR, MAbsoluteY), 0x59)
    , ((EOR, MIndirectX), 0x41), ((EOR, MIndirectY), 0x51)
    -- INC
    , ((INC, MZeroPage), 0xE6), ((INC, MZeroPageX), 0xF6)
    , ((INC, MAbsolute), 0xEE), ((INC, MAbsoluteX), 0xFE)
    -- INX, INY
    , ((INX, MImplied), 0xE8), ((INY, MImplied), 0xC8)
    -- JMP
    , ((JMP, MAbsolute), 0x4C), ((JMP, MIndirect), 0x6C)
    -- JSR
    , ((JSR, MAbsolute), 0x20)
    -- LDA
    , ((LDA, MImmediate), 0xA9), ((LDA, MZeroPage), 0xA5), ((LDA, MZeroPageX), 0xB5)
    , ((LDA, MAbsolute),  0xAD), ((LDA, MAbsoluteX), 0xBD), ((LDA, MAbsoluteY), 0xB9)
    , ((LDA, MIndirectX), 0xA1), ((LDA, MIndirectY), 0xB1)
    -- LDX
    , ((LDX, MImmediate), 0xA2), ((LDX, MZeroPage), 0xA6), ((LDX, MZeroPageY), 0xB6)
    , ((LDX, MAbsolute),  0xAE), ((LDX, MAbsoluteY), 0xBE)
    -- LDY
    , ((LDY, MImmediate), 0xA0), ((LDY, MZeroPage), 0xA4), ((LDY, MZeroPageX), 0xB4)
    , ((LDY, MAbsolute),  0xAC), ((LDY, MAbsoluteX), 0xBC)
    -- LSR
    , ((LSR, MAccumulator), 0x4A)
    , ((LSR, MZeroPage), 0x46), ((LSR, MZeroPageX), 0x56)
    , ((LSR, MAbsolute), 0x4E), ((LSR, MAbsoluteX), 0x5E)
    -- NOP
    , ((NOP, MImplied), 0xEA)
    -- ORA
    , ((ORA, MImmediate), 0x09), ((ORA, MZeroPage), 0x05), ((ORA, MZeroPageX), 0x15)
    , ((ORA, MAbsolute),  0x0D), ((ORA, MAbsoluteX), 0x1D), ((ORA, MAbsoluteY), 0x19)
    , ((ORA, MIndirectX), 0x01), ((ORA, MIndirectY), 0x11)
    -- PHA, PHP, PLA, PLP
    , ((PHA, MImplied), 0x48), ((PHP, MImplied), 0x08)
    , ((PLA, MImplied), 0x68), ((PLP, MImplied), 0x28)
    -- ROL
    , ((ROL, MAccumulator), 0x2A)
    , ((ROL, MZeroPage), 0x26), ((ROL, MZeroPageX), 0x36)
    , ((ROL, MAbsolute), 0x2E), ((ROL, MAbsoluteX), 0x3E)
    -- ROR
    , ((ROR, MAccumulator), 0x6A)
    , ((ROR, MZeroPage), 0x66), ((ROR, MZeroPageX), 0x76)
    , ((ROR, MAbsolute), 0x6E), ((ROR, MAbsoluteX), 0x7E)
    -- RTI, RTS
    , ((RTI, MImplied), 0x40), ((RTS, MImplied), 0x60)
    -- SBC
    , ((SBC, MImmediate), 0xE9), ((SBC, MZeroPage), 0xE5), ((SBC, MZeroPageX), 0xF5)
    , ((SBC, MAbsolute),  0xED), ((SBC, MAbsoluteX), 0xFD), ((SBC, MAbsoluteY), 0xF9)
    , ((SBC, MIndirectX), 0xE1), ((SBC, MIndirectY), 0xF1)
    -- SEC, SED, SEI
    , ((SEC, MImplied), 0x38), ((SED, MImplied), 0xF8), ((SEI, MImplied), 0x78)
    -- STA
    , ((STA, MZeroPage), 0x85), ((STA, MZeroPageX), 0x95)
    , ((STA, MAbsolute), 0x8D), ((STA, MAbsoluteX), 0x9D), ((STA, MAbsoluteY), 0x99)
    , ((STA, MIndirectX), 0x81), ((STA, MIndirectY), 0x91)
    -- STX
    , ((STX, MZeroPage), 0x86), ((STX, MZeroPageY), 0x96), ((STX, MAbsolute), 0x8E)
    -- STY
    , ((STY, MZeroPage), 0x84), ((STY, MZeroPageX), 0x94), ((STY, MAbsolute), 0x8C)
    -- TAX, TAY, TSX, TXA, TXS, TYA
    , ((TAX, MImplied), 0xAA), ((TAY, MImplied), 0xA8)
    , ((TSX, MImplied), 0xBA), ((TXA, MImplied), 0x8A)
    , ((TXS, MImplied), 0x9A), ((TYA, MImplied), 0x98)
    ]

opcodeFor :: HasCallStack => Opcode -> Mode -> Word8
opcodeFor opc mode =
    case Map.lookup (opc, mode) opcodeTable of
        Just byte -> byte
        Nothing   -> error $ "Invalid addressing mode "
                          ++ show mode ++ " for " ++ show opc

-- ---------------------------------------------------------------------------
-- Instruction emission helpers
-- ---------------------------------------------------------------------------

emitOp :: Operand op => Opcode -> op -> ASM ()
emitOp opc op = emit (opcodeFor opc (operandMode op) : operandBytes op)

emitImplied :: Opcode -> ASM ()
emitImplied opc = emit [opcodeFor opc MImplied]

emitAccum :: Opcode -> ASM ()
emitAccum opc = emit [opcodeFor opc MAccumulator]

emitBranch :: Opcode -> Word16 -> ASM ()
emitBranch opc target = do
    pc <- label
    -- The range check is embedded in the byte value (not in a 'when' guard)
    -- so it stays lazy and works correctly inside mdo/MonadFix blocks where
    -- the target address is a forward reference resolved after assembly.
    let diff = fromIntegral target - fromIntegral pc - 2 :: Int
        byte | diff < -128 || diff > 127 =
                 error $ show opc ++ " branch out of range: offset " ++ show diff
                      ++ " (PC=$" ++ showHex16 pc
                      ++ ", target=$" ++ showHex16 target ++ ")"
             | otherwise = fromIntegral diff
    emit [opcodeFor opc MRelative, byte]
  where
    showHex16 :: Word16 -> String
    showHex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s

-- ---------------------------------------------------------------------------
-- Operand instructions
-- ---------------------------------------------------------------------------

lda :: Operand op => op -> ASM ()
lda = emitOp LDA

ldx :: Operand op => op -> ASM ()
ldx = emitOp LDX

ldy :: Operand op => op -> ASM ()
ldy = emitOp LDY

sta :: Operand op => op -> ASM ()
sta = emitOp STA

stx :: Operand op => op -> ASM ()
stx = emitOp STX

sty :: Operand op => op -> ASM ()
sty = emitOp STY

adc :: Operand op => op -> ASM ()
adc = emitOp ADC

sbc :: Operand op => op -> ASM ()
sbc = emitOp SBC

and_ :: Operand op => op -> ASM ()
and_ = emitOp AND

ora :: Operand op => op -> ASM ()
ora = emitOp ORA

eor :: Operand op => op -> ASM ()
eor = emitOp EOR

cmp :: Operand op => op -> ASM ()
cmp = emitOp CMP

cpx :: Operand op => op -> ASM ()
cpx = emitOp CPX

cpy :: Operand op => op -> ASM ()
cpy = emitOp CPY

asl :: Operand op => op -> ASM ()
asl = emitOp ASL

lsr :: Operand op => op -> ASM ()
lsr = emitOp LSR

rol :: Operand op => op -> ASM ()
rol = emitOp ROL

ror :: Operand op => op -> ASM ()
ror = emitOp ROR

inc :: Operand op => op -> ASM ()
inc = emitOp INC

dec :: Operand op => op -> ASM ()
dec = emitOp DEC

bit :: Operand op => op -> ASM ()
bit = emitOp BIT

-- ---------------------------------------------------------------------------
-- Implied instructions
-- ---------------------------------------------------------------------------

brk, clc, cld, cli, clv :: ASM ()
brk = emitImplied BRK
clc = emitImplied CLC
cld = emitImplied CLD
cli = emitImplied CLI
clv = emitImplied CLV

dex, dey, inx, iny, nop :: ASM ()
dex = emitImplied DEX
dey = emitImplied DEY
inx = emitImplied INX
iny = emitImplied INY
nop = emitImplied NOP

pha, php, pla, plp :: ASM ()
pha = emitImplied PHA
php = emitImplied PHP
pla = emitImplied PLA
plp = emitImplied PLP

rti, rts :: ASM ()
rti = emitImplied RTI
rts = emitImplied RTS

sec, sed, sei :: ASM ()
sec = emitImplied SEC
sed = emitImplied SED
sei = emitImplied SEI

tax, tay, tsx, txa, txs, tya :: ASM ()
tax = emitImplied TAX
tay = emitImplied TAY
tsx = emitImplied TSX
txa = emitImplied TXA
txs = emitImplied TXS
tya = emitImplied TYA

-- ---------------------------------------------------------------------------
-- Accumulator instructions
-- ---------------------------------------------------------------------------

asl_a, lsr_a, rol_a, ror_a :: ASM ()
asl_a = emitAccum ASL
lsr_a = emitAccum LSR
rol_a = emitAccum ROL
ror_a = emitAccum ROR

-- ---------------------------------------------------------------------------
-- Branch instructions
-- ---------------------------------------------------------------------------

bcc, bcs, beq, bmi, bne, bpl, bvc, bvs :: Word16 -> ASM ()
bcc = emitBranch BCC
bcs = emitBranch BCS
beq = emitBranch BEQ
bmi = emitBranch BMI
bne = emitBranch BNE
bpl = emitBranch BPL
bvc = emitBranch BVC
bvs = emitBranch BVS

-- ---------------------------------------------------------------------------
-- Jump instructions
-- ---------------------------------------------------------------------------

jmp :: Word16 -> ASM ()
jmp addr = emit [opcodeFor JMP MAbsolute, lo addr, hi addr]

jmp_ind :: Word16 -> ASM ()
jmp_ind addr = emit [opcodeFor JMP MIndirect, lo addr, hi addr]

jmpInd :: Word16 -> ASM ()
jmpInd = jmp_ind

jsr :: Word16 -> ASM ()
jsr addr = emit [opcodeFor JSR MAbsolute, lo addr, hi addr]
