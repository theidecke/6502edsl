{-# LANGUAGE RecursiveDo #-}

module Test.Label (tests) where

import Data.Word (Word8, Word16)

import Asm.Monad (Label(..), MonadASM, label)
import Asm.Mos6502
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)
import Test.Helpers

-- | Helper: assemble an mdo block that creates a label and uses it with an
-- opcode, then compare against the expected encoding with Absolute mode.
-- The label lands at address 0x0003 (after the 3-byte instruction).
labelAbs :: (forall m. MonadASM m => Label -> m ()) -> [Word8]
labelAbs f = asm $ mdo
    f target
    target <- label
    pure ()

-- | Same but for (Label, X) indexed addressing.
labelAbsX :: (forall m. MonadASM m => (Label, X_) -> m ()) -> [Word8]
labelAbsX f = asm $ mdo
    f (target, X)
    target <- label
    pure ()

-- | Same but for (Label, Y) indexed addressing.
labelAbsY :: (forall m. MonadASM m => (Label, Y_) -> m ()) -> [Word8]
labelAbsY f = asm $ mdo
    f (target, Y)
    target <- label
    pure ()

-- | Expected encoding for a 3-byte absolute instruction targeting addr 0x0003.
expectedAbs :: Opcode -> [Word8]
expectedAbs op = encode (Instruction op (Absolute 0x0003))

expectedAbsX :: Opcode -> [Word8]
expectedAbsX op = encode (Instruction op (AbsoluteX 0x0003))

expectedAbsY :: Opcode -> [Word8]
expectedAbsY op = encode (Instruction op (AbsoluteY 0x0003))

-- ---------------------------------------------------------------------------
-- Label → Absolute (21 opcodes)
-- ---------------------------------------------------------------------------

prop_labelAbs_lda :: Bool
prop_labelAbs_lda = labelAbs lda == expectedAbs LDA

prop_labelAbs_ldx :: Bool
prop_labelAbs_ldx = labelAbs ldx == expectedAbs LDX

prop_labelAbs_ldy :: Bool
prop_labelAbs_ldy = labelAbs ldy == expectedAbs LDY

prop_labelAbs_sta :: Bool
prop_labelAbs_sta = labelAbs sta == expectedAbs STA

prop_labelAbs_stx :: Bool
prop_labelAbs_stx = labelAbs stx == expectedAbs STX

prop_labelAbs_sty :: Bool
prop_labelAbs_sty = labelAbs sty == expectedAbs STY

prop_labelAbs_adc :: Bool
prop_labelAbs_adc = labelAbs adc == expectedAbs ADC

prop_labelAbs_sbc :: Bool
prop_labelAbs_sbc = labelAbs sbc == expectedAbs SBC

prop_labelAbs_and :: Bool
prop_labelAbs_and = labelAbs and_ == expectedAbs AND

prop_labelAbs_ora :: Bool
prop_labelAbs_ora = labelAbs ora == expectedAbs ORA

prop_labelAbs_eor :: Bool
prop_labelAbs_eor = labelAbs eor == expectedAbs EOR

prop_labelAbs_cmp :: Bool
prop_labelAbs_cmp = labelAbs cmp == expectedAbs CMP

prop_labelAbs_cpx :: Bool
prop_labelAbs_cpx = labelAbs cpx == expectedAbs CPX

prop_labelAbs_cpy :: Bool
prop_labelAbs_cpy = labelAbs cpy == expectedAbs CPY

prop_labelAbs_asl :: Bool
prop_labelAbs_asl = labelAbs asl == expectedAbs ASL

prop_labelAbs_lsr :: Bool
prop_labelAbs_lsr = labelAbs lsr == expectedAbs LSR

prop_labelAbs_rol :: Bool
prop_labelAbs_rol = labelAbs rol == expectedAbs ROL

prop_labelAbs_ror :: Bool
prop_labelAbs_ror = labelAbs ror == expectedAbs ROR

prop_labelAbs_inc :: Bool
prop_labelAbs_inc = labelAbs inc == expectedAbs INC

prop_labelAbs_dec :: Bool
prop_labelAbs_dec = labelAbs dec == expectedAbs DEC

prop_labelAbs_bit :: Bool
prop_labelAbs_bit = labelAbs bit == expectedAbs BIT

-- ---------------------------------------------------------------------------
-- (Label, X) → AbsoluteX (14 opcodes)
-- ---------------------------------------------------------------------------

prop_labelAbsX_lda :: Bool
prop_labelAbsX_lda = labelAbsX lda == expectedAbsX LDA

prop_labelAbsX_sta :: Bool
prop_labelAbsX_sta = labelAbsX sta == expectedAbsX STA

prop_labelAbsX_adc :: Bool
prop_labelAbsX_adc = labelAbsX adc == expectedAbsX ADC

prop_labelAbsX_sbc :: Bool
prop_labelAbsX_sbc = labelAbsX sbc == expectedAbsX SBC

prop_labelAbsX_and :: Bool
prop_labelAbsX_and = labelAbsX and_ == expectedAbsX AND

prop_labelAbsX_ora :: Bool
prop_labelAbsX_ora = labelAbsX ora == expectedAbsX ORA

prop_labelAbsX_eor :: Bool
prop_labelAbsX_eor = labelAbsX eor == expectedAbsX EOR

prop_labelAbsX_cmp :: Bool
prop_labelAbsX_cmp = labelAbsX cmp == expectedAbsX CMP

prop_labelAbsX_asl :: Bool
prop_labelAbsX_asl = labelAbsX asl == expectedAbsX ASL

prop_labelAbsX_lsr :: Bool
prop_labelAbsX_lsr = labelAbsX lsr == expectedAbsX LSR

prop_labelAbsX_rol :: Bool
prop_labelAbsX_rol = labelAbsX rol == expectedAbsX ROL

prop_labelAbsX_ror :: Bool
prop_labelAbsX_ror = labelAbsX ror == expectedAbsX ROR

prop_labelAbsX_inc :: Bool
prop_labelAbsX_inc = labelAbsX inc == expectedAbsX INC

prop_labelAbsX_dec :: Bool
prop_labelAbsX_dec = labelAbsX dec == expectedAbsX DEC

-- ---------------------------------------------------------------------------
-- (Label, Y) → AbsoluteY (8 opcodes)
-- ---------------------------------------------------------------------------

prop_labelAbsY_lda :: Bool
prop_labelAbsY_lda = labelAbsY lda == expectedAbsY LDA

prop_labelAbsY_ldx :: Bool
prop_labelAbsY_ldx = labelAbsY ldx == expectedAbsY LDX

prop_labelAbsY_sta :: Bool
prop_labelAbsY_sta = labelAbsY sta == expectedAbsY STA

prop_labelAbsY_adc :: Bool
prop_labelAbsY_adc = labelAbsY adc == expectedAbsY ADC

prop_labelAbsY_sbc :: Bool
prop_labelAbsY_sbc = labelAbsY sbc == expectedAbsY SBC

prop_labelAbsY_and :: Bool
prop_labelAbsY_and = labelAbsY and_ == expectedAbsY AND

prop_labelAbsY_ora :: Bool
prop_labelAbsY_ora = labelAbsY ora == expectedAbsY ORA

prop_labelAbsY_eor :: Bool
prop_labelAbsY_eor = labelAbsY eor == expectedAbsY EOR

prop_labelAbsY_cmp :: Bool
prop_labelAbsY_cmp = labelAbsY cmp == expectedAbsY CMP

-- ---------------------------------------------------------------------------
-- Label equivalence with Word16 (parametric)
-- ---------------------------------------------------------------------------

-- | A Label with a given address produces the same bytes as a bare Word16.
prop_labelEquivWord16 :: Word16 -> Bool
prop_labelEquivWord16 w =
    let l = Label w Nothing
    in  asm (lda l) == asm (lda w)

prop_labelXEquivWord16X :: Word16 -> Bool
prop_labelXEquivWord16X w =
    let l = Label w Nothing
    in  asm (lda (l, X)) == asm (lda (w, X))

prop_labelYEquivWord16Y :: Word16 -> Bool
prop_labelYEquivWord16Y w =
    let l = Label w Nothing
    in  asm (lda (l, Y)) == asm (lda (w, Y))

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "Label → Absolute"
    , checkOnce "lda label"  prop_labelAbs_lda
    , checkOnce "ldx label"  prop_labelAbs_ldx
    , checkOnce "ldy label"  prop_labelAbs_ldy
    , checkOnce "sta label"  prop_labelAbs_sta
    , checkOnce "stx label"  prop_labelAbs_stx
    , checkOnce "sty label"  prop_labelAbs_sty
    , checkOnce "adc label"  prop_labelAbs_adc
    , checkOnce "sbc label"  prop_labelAbs_sbc
    , checkOnce "and_ label" prop_labelAbs_and
    , checkOnce "ora label"  prop_labelAbs_ora
    , checkOnce "eor label"  prop_labelAbs_eor
    , checkOnce "cmp label"  prop_labelAbs_cmp
    , checkOnce "cpx label"  prop_labelAbs_cpx
    , checkOnce "cpy label"  prop_labelAbs_cpy
    , checkOnce "asl label"  prop_labelAbs_asl
    , checkOnce "lsr label"  prop_labelAbs_lsr
    , checkOnce "rol label"  prop_labelAbs_rol
    , checkOnce "ror label"  prop_labelAbs_ror
    , checkOnce "inc label"  prop_labelAbs_inc
    , checkOnce "dec label"  prop_labelAbs_dec
    , checkOnce "bit label"  prop_labelAbs_bit

    , section "(Label, X) → AbsoluteX"
    , checkOnce "lda (label,X)"  prop_labelAbsX_lda
    , checkOnce "sta (label,X)"  prop_labelAbsX_sta
    , checkOnce "adc (label,X)"  prop_labelAbsX_adc
    , checkOnce "sbc (label,X)"  prop_labelAbsX_sbc
    , checkOnce "and_ (label,X)" prop_labelAbsX_and
    , checkOnce "ora (label,X)"  prop_labelAbsX_ora
    , checkOnce "eor (label,X)"  prop_labelAbsX_eor
    , checkOnce "cmp (label,X)"  prop_labelAbsX_cmp
    , checkOnce "asl (label,X)"  prop_labelAbsX_asl
    , checkOnce "lsr (label,X)"  prop_labelAbsX_lsr
    , checkOnce "rol (label,X)"  prop_labelAbsX_rol
    , checkOnce "ror (label,X)"  prop_labelAbsX_ror
    , checkOnce "inc (label,X)"  prop_labelAbsX_inc
    , checkOnce "dec (label,X)"  prop_labelAbsX_dec

    , section "(Label, Y) → AbsoluteY"
    , checkOnce "lda (label,Y)"  prop_labelAbsY_lda
    , checkOnce "ldx (label,Y)"  prop_labelAbsY_ldx
    , checkOnce "sta (label,Y)"  prop_labelAbsY_sta
    , checkOnce "adc (label,Y)"  prop_labelAbsY_adc
    , checkOnce "sbc (label,Y)"  prop_labelAbsY_sbc
    , checkOnce "and_ (label,Y)" prop_labelAbsY_and
    , checkOnce "ora (label,Y)"  prop_labelAbsY_ora
    , checkOnce "eor (label,Y)"  prop_labelAbsY_eor
    , checkOnce "cmp (label,Y)"  prop_labelAbsY_cmp

    , section "Label ≡ Word16 equivalence"
    , check "Label w ≡ bare Word16"       prop_labelEquivWord16
    , check "(Label w, X) ≡ (Word16, X)"  prop_labelXEquivWord16X
    , check "(Label w, Y) ≡ (Word16, Y)"  prop_labelYEquivWord16Y
    ]
