module Test.Instructions (tests) where

import Data.Word (Word8, Word16)

import Asm.Monad (assemble)
import Asm.Mos6502
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)
import Test.Helpers

-- ---------------------------------------------------------------------------
-- EDSL instruction functions (9 props)
-- ---------------------------------------------------------------------------

prop_ldaImm :: Imm -> Bool
prop_ldaImm v@(Imm b) = asm (lda v) == encode (Instruction LDA (Immediate b))

prop_ldaZP :: ZP -> Bool
prop_ldaZP v@(ZP b) = asm (lda v) == encode (Instruction LDA (ZeroPage b))

prop_ldaAbs :: Abs -> Bool
prop_ldaAbs v@(Abs w) = asm (lda v) == encode (Instruction LDA (Absolute w))

prop_staZPX :: ZPX -> Bool
prop_staZPX v@(ZPX b) = asm (sta v) == encode (Instruction STA (ZeroPageX b))

prop_ldxAbsY :: AbsY -> Bool
prop_ldxAbsY v@(AbsY w) = asm (ldx v) == encode (Instruction LDX (AbsoluteY w))

prop_ldaIndX :: IndX -> Bool
prop_ldaIndX v@(IndX b) = asm (lda v) == encode (Instruction LDA (IndirectX b))

prop_ldaIndY :: IndY -> Bool
prop_ldaIndY v@(IndY b) = asm (lda v) == encode (Instruction LDA (IndirectY b))

prop_nopImplied :: Bool
prop_nopImplied = asm nop == encode (Instruction NOP Implied)

prop_aslAccumulator :: Bool
prop_aslAccumulator = asm asl_a == encode (Instruction ASL Accumulator)

-- ---------------------------------------------------------------------------
-- Addressing mode sugar (19 props)
-- ---------------------------------------------------------------------------

prop_sugarImmediate :: Word8 -> Bool
prop_sugarImmediate v = asm (lda # v) == asm (lda (Imm v))

prop_sugarBareWord8 :: Word8 -> Bool
prop_sugarBareWord8 w = asm (lda w) == asm (lda (ZP w))

prop_sugarBareWord16 :: Word16 -> Bool
prop_sugarBareWord16 w = asm (lda w) == asm (lda (Abs w))

prop_sugarAccumulatorAsl :: Bool
prop_sugarAccumulatorAsl = asm (asl A) == asm asl_a

prop_sugarAccumulatorLsr :: Bool
prop_sugarAccumulatorLsr = asm (lsr A) == asm lsr_a

prop_sugarAccumulatorRol :: Bool
prop_sugarAccumulatorRol = asm (rol A) == asm rol_a

prop_sugarAccumulatorRor :: Bool
prop_sugarAccumulatorRor = asm (ror A) == asm ror_a

prop_sugarZPX :: Word8 -> Bool
prop_sugarZPX w = asm (lda (w, X)) == asm (lda (ZPX w))

prop_sugarZPY :: Word8 -> Bool
prop_sugarZPY w = asm (ldx (w, Y)) == asm (ldx (ZPY w))

prop_sugarAbsX :: Word16 -> Bool
prop_sugarAbsX w = asm (lda (w, X)) == asm (lda (AbsX w))

prop_sugarAbsY :: Word16 -> Bool
prop_sugarAbsY w = asm (lda (w, Y)) == asm (lda (AbsY w))

prop_sugarIndY :: Word8 -> Bool
prop_sugarIndY w = asm (lda (w ! Y)) == asm (lda (IndY w))

prop_sugarIndX :: Word8 -> Bool
prop_sugarIndX w = asm (lda (w ! X)) == asm (lda (IndX w))

prop_sugarVar8 :: Bool
prop_sugarVar8 =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            v <- allocVar8
            lda v
    in  bytes == encode (Instruction LDA (ZeroPage 0x10))

prop_sugarPtrIndY :: Bool
prop_sugarPtrIndY =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            lda (p ! Y)
    in  bytes == encode (Instruction LDA (IndirectY 0x10))

prop_sugarPtrArith :: Bool
prop_sugarPtrArith =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            sta p
            sta (p + 1)
    in  bytes == encode (Instruction STA (ZeroPage 0x10))
             ++ encode (Instruction STA (ZeroPage 0x11))

prop_sugarVar16Helpers :: Bool
prop_sugarVar16Helpers =
    let cfg = zpConfig [0x20 .. 0x29]
        (_, bytes) = assemble cfg $ do
            v <- allocVar16
            lda (lo16 v)
            lda (hi16 v)
    in  bytes == encode (Instruction LDA (ZeroPage 0x20))
             ++ encode (Instruction LDA (ZeroPage 0x21))

prop_sugarAllocators :: Bool
prop_sugarAllocators =
    let cfg = zpConfig [0x02 .. 0x0A]
        ((v8, v16, p), _) = assemble cfg $ do
            a <- allocVar8   -- 1 byte at 0x02
            b <- allocVar16  -- 2 bytes at 0x03
            c <- allocPtr    -- 2 bytes at 0x05
            pure (a, b, c)
    in  v8 == Var8 0x02 && v16 == Var16 0x03 && p == Ptr 0x05

prop_sugarVar8X :: Bool
prop_sugarVar8X =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            v <- allocVar8
            lda (v, X)
    in  bytes == encode (Instruction LDA (ZeroPageX 0x10))

prop_sugarPtrIndX :: Bool
prop_sugarPtrIndX =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            lda (p ! X)
    in  bytes == encode (Instruction LDA (IndirectX 0x10))

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "EDSL instruction functions"
    , check "lda (Imm v)"   prop_ldaImm
    , check "lda (ZP v)"    prop_ldaZP
    , check "lda (Abs v)"   prop_ldaAbs
    , check "sta (ZPX v)"   prop_staZPX
    , check "ldx (AbsY v)"  prop_ldxAbsY
    , check "lda (IndX v)"  prop_ldaIndX
    , check "lda (IndY v)"  prop_ldaIndY
    , checkOnce "nop (implied)"      prop_nopImplied
    , checkOnce "asl_a (accumulator)" prop_aslAccumulator

    , section "Addressing mode sugar"
    , check "# immediate"           prop_sugarImmediate
    , check "bare Word8 → ZP"       prop_sugarBareWord8
    , check "bare Word16 → Abs"     prop_sugarBareWord16
    , checkOnce "asl A == asl_a"    prop_sugarAccumulatorAsl
    , checkOnce "lsr A == lsr_a"    prop_sugarAccumulatorLsr
    , checkOnce "rol A == rol_a"    prop_sugarAccumulatorRol
    , checkOnce "ror A == ror_a"    prop_sugarAccumulatorRor
    , check "(Word8, X) → ZPX"     prop_sugarZPX
    , check "(Word8, Y) → ZPY"     prop_sugarZPY
    , check "(Word16, X) → AbsX"   prop_sugarAbsX
    , check "(Word16, Y) → AbsY"   prop_sugarAbsY
    , check "Word8 ! Y → IndY"     prop_sugarIndY
    , check "Word8 ! X → IndX"     prop_sugarIndX
    , checkOnce "Var8 → ZP"         prop_sugarVar8
    , checkOnce "Ptr ! Y → IndY"    prop_sugarPtrIndY
    , checkOnce "Ptr arithmetic"    prop_sugarPtrArith
    , checkOnce "Var16 lo16/hi16"   prop_sugarVar16Helpers
    , checkOnce "typed allocators"  prop_sugarAllocators
    , checkOnce "Var8 tuple (v,X)"  prop_sugarVar8X
    , checkOnce "Ptr ! X → IndX"    prop_sugarPtrIndX
    ]
