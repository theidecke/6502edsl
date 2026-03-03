{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Mos6502
    ( -- * Operand newtypes
      Imm(..), ZP(..), ZPX(..), ZPY(..)
    , Abs(..), AbsX(..), AbsY(..)
    , Ind(..), IndX(..), IndY(..)
      -- * Operand typeclass
    , Operand(..)
      -- * Singleton types
    , X_(..), Y_(..), A_(..)
      -- * Variable types (zero-page)
    , Var8(..), Var16(..), Ptr(..)
      -- * Memory location types (absolute)
    , Mem8(..), Mem16(..), MemPtr(..)
      -- * 16-bit location typeclass
    , Loc16(..)
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

import Data.Word (Word8, Word16)
import Numeric (showHex)

import Asm.Monad (MonadASM(..), MonadZPAlloc(..), Label(..), ToAddr(..))
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..))

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
    toAddrMode :: a -> AddressingMode

instance Operand Imm  where toAddrMode (Imm v)  = Immediate v
instance Operand ZP   where toAddrMode (ZP a)   = ZeroPage a
instance Operand ZPX  where toAddrMode (ZPX a)  = ZeroPageX a
instance Operand ZPY  where toAddrMode (ZPY a)  = ZeroPageY a
instance Operand Abs  where toAddrMode (Abs w)  = Absolute w
instance Operand AbsX where toAddrMode (AbsX w) = AbsoluteX w
instance Operand AbsY where toAddrMode (AbsY w) = AbsoluteY w
instance Operand Ind  where toAddrMode (Ind w)  = Indirect w
instance Operand IndX where toAddrMode (IndX a) = IndirectX a
instance Operand IndY where toAddrMode (IndY a) = IndirectY a

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

-- ---------------------------------------------------------------------------
-- Absolute memory location types
-- ---------------------------------------------------------------------------

newtype Mem8   = Mem8   Word16 deriving (Show, Eq)
newtype Mem16  = Mem16  Word16 deriving (Show, Eq)
newtype MemPtr = MemPtr Word16 deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Loc16 typeclass — 16-bit memory locations (ZP or absolute)
-- ---------------------------------------------------------------------------

class Operand (Byte a) => Loc16 a where
    type Byte a
    lo16 :: a -> Byte a
    hi16 :: a -> Byte a

instance Loc16 Var16 where
    type Byte Var16 = Var8
    lo16 (Var16 a) = Var8 a
    hi16 (Var16 a) = Var8 (a + 1)

instance Loc16 Ptr where
    type Byte Ptr = Var8
    lo16 (Ptr a) = Var8 a
    hi16 (Ptr a) = Var8 (a + 1)

instance Loc16 Mem16 where
    type Byte Mem16 = Mem8
    lo16 (Mem16 a) = Mem8 a
    hi16 (Mem16 a) = Mem8 (a + 1)

instance Loc16 MemPtr where
    type Byte MemPtr = Mem8
    lo16 (MemPtr a) = Mem8 a
    hi16 (MemPtr a) = Mem8 (a + 1)

-- ---------------------------------------------------------------------------
-- Operand instances for bare types and tuples
-- ---------------------------------------------------------------------------

instance Operand Word8  where toAddrMode w            = ZeroPage w
instance Operand Word16 where toAddrMode w            = Absolute w
instance Operand A_     where toAddrMode _             = Accumulator
instance Operand Var8   where toAddrMode (Var8 a)     = ZeroPage a
instance Operand Ptr    where toAddrMode (Ptr a)      = ZeroPage a
instance Operand Mem8   where toAddrMode (Mem8 a)     = Absolute a
instance Operand MemPtr where toAddrMode (MemPtr a)   = Absolute a
instance Operand Label  where toAddrMode l            = Absolute (labelAddr l)

instance Operand (Word8,  X_) where toAddrMode (w, _) = ZeroPageX w
instance Operand (Word8,  Y_) where toAddrMode (w, _) = ZeroPageY w
instance Operand (Word16, X_) where toAddrMode (w, _) = AbsoluteX w
instance Operand (Word16, Y_) where toAddrMode (w, _) = AbsoluteY w
instance Operand (Label,  X_) where toAddrMode (l, _) = AbsoluteX (labelAddr l)
instance Operand (Label,  Y_) where toAddrMode (l, _) = AbsoluteY (labelAddr l)
instance Operand (Var8,   X_) where toAddrMode (Var8 a, _) = ZeroPageX a
instance Operand (Var8,   Y_) where toAddrMode (Var8 a, _) = ZeroPageY a
instance Operand (Ptr,    X_) where toAddrMode (Ptr a, _)  = ZeroPageX a
instance Operand (Ptr,    Y_) where toAddrMode (Ptr a, _)  = ZeroPageY a

-- ---------------------------------------------------------------------------
-- (#) operator — immediate mode sugar
-- ---------------------------------------------------------------------------

infixl 8 #

(#) :: (Imm -> m ()) -> Word8 -> m ()
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

allocVar8 :: MonadZPAlloc m => m Var8
allocVar8 = Var8 <$> allocZP 1

allocVar16 :: MonadZPAlloc m => m Var16
allocVar16 = Var16 <$> allocZP 2

allocPtr :: MonadZPAlloc m => m Ptr
allocPtr = Ptr <$> allocZP 2

-- ---------------------------------------------------------------------------
-- Instruction emission helpers
-- ---------------------------------------------------------------------------

emitOp :: (MonadASM m, Operand op) => Opcode -> op -> m ()
emitOp opc op = emitInstruction (Instruction opc (toAddrMode op))

emitImplied :: MonadASM m => Opcode -> m ()
emitImplied opc = emitInstruction (Instruction opc Implied)

emitAccum :: MonadASM m => Opcode -> m ()
emitAccum opc = emitInstruction (Instruction opc Accumulator)

emitBranch :: (MonadASM m, ToAddr a) => Opcode -> a -> m ()
emitBranch opc target = do
    pc <- currentPC
    let diff = fromIntegral (toAddr target) - fromIntegral pc - 2 :: Int
        byte | diff < -128 || diff > 127 =
                 error $ show opc ++ " branch out of range: offset " ++ show diff
                      ++ " (PC=$" ++ showHex16 pc
                      ++ ", target=$" ++ showHex16 (toAddr target) ++ ")"
             | otherwise = fromIntegral diff
    emitInstruction (Instruction opc (Relative byte))
  where
    showHex16 :: Word16 -> String
    showHex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s

-- ---------------------------------------------------------------------------
-- Operand instructions
-- ---------------------------------------------------------------------------

lda :: (MonadASM m, Operand op) => op -> m ()
lda = emitOp LDA

ldx :: (MonadASM m, Operand op) => op -> m ()
ldx = emitOp LDX

ldy :: (MonadASM m, Operand op) => op -> m ()
ldy = emitOp LDY

sta :: (MonadASM m, Operand op) => op -> m ()
sta = emitOp STA

stx :: (MonadASM m, Operand op) => op -> m ()
stx = emitOp STX

sty :: (MonadASM m, Operand op) => op -> m ()
sty = emitOp STY

adc :: (MonadASM m, Operand op) => op -> m ()
adc = emitOp ADC

sbc :: (MonadASM m, Operand op) => op -> m ()
sbc = emitOp SBC

and_ :: (MonadASM m, Operand op) => op -> m ()
and_ = emitOp AND

ora :: (MonadASM m, Operand op) => op -> m ()
ora = emitOp ORA

eor :: (MonadASM m, Operand op) => op -> m ()
eor = emitOp EOR

cmp :: (MonadASM m, Operand op) => op -> m ()
cmp = emitOp CMP

cpx :: (MonadASM m, Operand op) => op -> m ()
cpx = emitOp CPX

cpy :: (MonadASM m, Operand op) => op -> m ()
cpy = emitOp CPY

asl :: (MonadASM m, Operand op) => op -> m ()
asl = emitOp ASL

lsr :: (MonadASM m, Operand op) => op -> m ()
lsr = emitOp LSR

rol :: (MonadASM m, Operand op) => op -> m ()
rol = emitOp ROL

ror :: (MonadASM m, Operand op) => op -> m ()
ror = emitOp ROR

inc :: (MonadASM m, Operand op) => op -> m ()
inc = emitOp INC

dec :: (MonadASM m, Operand op) => op -> m ()
dec = emitOp DEC

bit :: (MonadASM m, Operand op) => op -> m ()
bit = emitOp BIT

-- ---------------------------------------------------------------------------
-- Implied instructions
-- ---------------------------------------------------------------------------

brk, clc, cld, cli, clv :: MonadASM m => m ()
brk = emitImplied BRK
clc = emitImplied CLC
cld = emitImplied CLD
cli = emitImplied CLI
clv = emitImplied CLV

dex, dey, inx, iny, nop :: MonadASM m => m ()
dex = emitImplied DEX
dey = emitImplied DEY
inx = emitImplied INX
iny = emitImplied INY
nop = emitImplied NOP

pha, php, pla, plp :: MonadASM m => m ()
pha = emitImplied PHA
php = emitImplied PHP
pla = emitImplied PLA
plp = emitImplied PLP

rti, rts :: MonadASM m => m ()
rti = emitImplied RTI
rts = emitImplied RTS

sec, sed, sei :: MonadASM m => m ()
sec = emitImplied SEC
sed = emitImplied SED
sei = emitImplied SEI

tax, tay, tsx, txa, txs, tya :: MonadASM m => m ()
tax = emitImplied TAX
tay = emitImplied TAY
tsx = emitImplied TSX
txa = emitImplied TXA
txs = emitImplied TXS
tya = emitImplied TYA

-- ---------------------------------------------------------------------------
-- Accumulator instructions
-- ---------------------------------------------------------------------------

asl_a, lsr_a, rol_a, ror_a :: MonadASM m => m ()
asl_a = emitAccum ASL
lsr_a = emitAccum LSR
rol_a = emitAccum ROL
ror_a = emitAccum ROR

-- ---------------------------------------------------------------------------
-- Branch instructions
-- ---------------------------------------------------------------------------

bcc, bcs, beq, bmi, bne, bpl, bvc, bvs :: (MonadASM m, ToAddr a) => a -> m ()
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

jmp :: (MonadASM m, ToAddr a) => a -> m ()
jmp target = emitInstruction (Instruction JMP (Absolute (toAddr target)))

jmp_ind :: (MonadASM m, ToAddr a) => a -> m ()
jmp_ind target = emitInstruction (Instruction JMP (Indirect (toAddr target)))

jmpInd :: (MonadASM m, ToAddr a) => a -> m ()
jmpInd = jmp_ind

jsr :: (MonadASM m, ToAddr a) => a -> m ()
jsr target = emitInstruction (Instruction JSR (Absolute (toAddr target)))
