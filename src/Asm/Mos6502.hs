{-# LANGUAGE FunctionalDependencies #-}

module Asm.Mos6502
    ( -- * Operand newtypes
      Imm(..), ZP(..), ZPX(..), ZPY(..)
    , Abs(..), AbsX(..), AbsY(..)
    , Ind(..), IndX(..), IndY(..)
      -- * Operand typeclass
    , Operand(..)
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

import Data.Word (Word8, Word16)
import Numeric (showHex)

import Asm.Monad (ASM, emit, currentPC, allocZP, ToAddr(..))
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)

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

lo16 :: Var16 -> Var8
lo16 (Var16 a) = Var8 a

hi16 :: Var16 -> Var8
hi16 (Var16 a) = Var8 (a + 1)

-- ---------------------------------------------------------------------------
-- Operand instances for bare types and tuples
-- ---------------------------------------------------------------------------

instance Operand Word8  where toAddrMode w          = ZeroPage w
instance Operand Word16 where toAddrMode w          = Absolute w
instance Operand A_     where toAddrMode _           = Accumulator
instance Operand Var8   where toAddrMode (Var8 a)   = ZeroPage a
instance Operand Ptr    where toAddrMode (Ptr a)    = ZeroPage a

instance Operand (Word8,  X_) where toAddrMode (w, _) = ZeroPageX w
instance Operand (Word8,  Y_) where toAddrMode (w, _) = ZeroPageY w
instance Operand (Word16, X_) where toAddrMode (w, _) = AbsoluteX w
instance Operand (Word16, Y_) where toAddrMode (w, _) = AbsoluteY w
instance Operand (Var8,   X_) where toAddrMode (Var8 a, _) = ZeroPageX a
instance Operand (Var8,   Y_) where toAddrMode (Var8 a, _) = ZeroPageY a
instance Operand (Ptr,    X_) where toAddrMode (Ptr a, _)  = ZeroPageX a
instance Operand (Ptr,    Y_) where toAddrMode (Ptr a, _)  = ZeroPageY a

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
-- Instruction emission helpers
-- ---------------------------------------------------------------------------

emitOp :: Operand op => Opcode -> op -> ASM ()
emitOp opc op = emit (encode (Instruction opc (toAddrMode op)))

emitImplied :: Opcode -> ASM ()
emitImplied opc = emit (encode (Instruction opc Implied))

emitAccum :: Opcode -> ASM ()
emitAccum opc = emit (encode (Instruction opc Accumulator))

emitBranch :: ToAddr a => Opcode -> a -> ASM ()
emitBranch opc target = do
    pc <- currentPC
    let diff = fromIntegral (toAddr target) - fromIntegral pc - 2 :: Int
        byte | diff < -128 || diff > 127 =
                 error $ show opc ++ " branch out of range: offset " ++ show diff
                      ++ " (PC=$" ++ showHex16 pc
                      ++ ", target=$" ++ showHex16 (toAddr target) ++ ")"
             | otherwise = fromIntegral diff
    emit (encode (Instruction opc (Relative byte)))
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

bcc, bcs, beq, bmi, bne, bpl, bvc, bvs :: ToAddr a => a -> ASM ()
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

jmp :: ToAddr a => a -> ASM ()
jmp target = emit (encode (Instruction JMP (Absolute (toAddr target))))

jmp_ind :: ToAddr a => a -> ASM ()
jmp_ind target = emit (encode (Instruction JMP (Indirect (toAddr target))))

jmpInd :: ToAddr a => a -> ASM ()
jmpInd = jmp_ind

jsr :: ToAddr a => a -> ASM ()
jsr target = emit (encode (Instruction JSR (Absolute (toAddr target))))
