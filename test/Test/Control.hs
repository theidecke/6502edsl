module Test.Control (tests) where

import Data.Word (Word8)

import Asm.Monad (ASM)
import Asm.Mos6502
import Asm.Mos6502.Control (if_, if_eq, if_ne, if_cs, if_cc, if_pl, if_mi, while_, for_x, for_y, loop_)
import Asm.Mos6502.Ops16 (add16, sub16, inc16, dec16, cmp16, mov16, load16, lshift16, rshift16)
import Test.Helpers

-- ---------------------------------------------------------------------------
-- Control flow (15 props)
-- ---------------------------------------------------------------------------

ifBytes :: (ASM () -> ASM () -> ASM ()) -> [Word8]
ifBytes f = asm (f nop nop)

prop_ifEqUsesBne :: Bool
prop_ifEqUsesBne = ifBytes if_eq == [0xD0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifNeUsesBeq :: Bool
prop_ifNeUsesBeq = ifBytes if_ne == [0xF0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifCsUsesBcc :: Bool
prop_ifCsUsesBcc = ifBytes if_cs == [0x90, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifCcUsesBcs :: Bool
prop_ifCcUsesBcs = ifBytes if_cc == [0xB0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifPlUsesBmi :: Bool
prop_ifPlUsesBmi = ifBytes if_pl == [0x30, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifMiUsesBpl :: Bool
prop_ifMiUsesBpl = ifBytes if_mi == [0x10, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_ifLargerThenBlock :: Bool
prop_ifLargerThenBlock =
    asm (if_eq (nop >> nop >> nop) nop)
    == [0xD0, 0x06, 0xEA, 0xEA, 0xEA, 0x4C, 0x09, 0x00, 0xEA]

prop_ifGeneralized :: Bool
prop_ifGeneralized =
    asm (if_ bvc nop nop) == [0x50, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

prop_forXBytes :: Bool
prop_forXBytes = asm (for_x 3 nop) == [0xA2, 0x03, 0xEA, 0xCA, 0xD0, 0xFC]

prop_forYBytes :: Bool
prop_forYBytes = asm (for_y 5 nop) == [0xA0, 0x05, 0xEA, 0x88, 0xD0, 0xFC]

prop_forXMultiByteBody :: Bool
prop_forXMultiByteBody =
    asm (for_x 2 (nop >> nop >> nop)) == [0xA2, 0x02, 0xEA, 0xEA, 0xEA, 0xCA, 0xD0, 0xFA]

prop_loopBytes :: Bool
prop_loopBytes = asm (loop_ nop) == [0xEA, 0x4C, 0x00, 0x00]

prop_whileBytes :: Bool
prop_whileBytes =
    asm (while_ beq (cmp # 0x05) nop)
    == [0xC9, 0x05, 0xF0, 0x04, 0xEA, 0x4C, 0x00, 0x00]

prop_ifEqMultiByteBodies :: Bool
prop_ifEqMultiByteBodies =
    asm (if_eq (lda # 0x01 >> sta (0x80 :: Word8))
               (lda # 0x02 >> sta (0x81 :: Word8)))
    == [ 0xD0, 0x07
       , 0xA9, 0x01, 0x85, 0x80
       , 0x4C, 0x0D, 0x00
       , 0xA9, 0x02, 0x85, 0x81
       ]

prop_whileMultiByteBody :: Bool
prop_whileMultiByteBody =
    asm (while_ beq (lda (0x80 :: Word8) >> cmp # 0x05)
                    (inc (0x80 :: Word8) >> inc (0x81 :: Word8)))
    == [ 0xA5, 0x80, 0xC9, 0x05
       , 0xF0, 0x07
       , 0xE6, 0x80, 0xE6, 0x81
       , 0x4C, 0x00, 0x00
       ]

-- ---------------------------------------------------------------------------
-- 16-bit operations (7 props)
-- ---------------------------------------------------------------------------

prop_load16Bytes :: Bool
prop_load16Bytes =
    asmZP (allocVar16 >>= \v -> load16 v 0x1234)
    == [0xA9, 0x34, 0x85, 0x02, 0xA9, 0x12, 0x85, 0x03]

prop_inc16Bytes :: Bool
prop_inc16Bytes =
    asmZP (allocVar16 >>= inc16)
    == [0xE6, 0x02, 0xD0, 0x02, 0xE6, 0x03]

prop_dec16Bytes :: Bool
prop_dec16Bytes =
    asmZP (allocVar16 >>= dec16)
    == [0xA5, 0x02, 0xD0, 0x02, 0xC6, 0x03, 0xC6, 0x02]

prop_add16Bytes :: Bool
prop_add16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16
            a   <- allocVar16
            b   <- allocVar16
            add16 dst a b
    in  bytes == [ 0x18
                 , 0xA5, 0x04, 0x65, 0x06, 0x85, 0x02
                 , 0xA5, 0x05, 0x65, 0x07, 0x85, 0x03
                 ]

prop_sub16Bytes :: Bool
prop_sub16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16
            a   <- allocVar16
            b   <- allocVar16
            sub16 dst a b
    in  bytes == [ 0x38
                 , 0xA5, 0x04, 0xE5, 0x06, 0x85, 0x02
                 , 0xA5, 0x05, 0xE5, 0x07, 0x85, 0x03
                 ]

prop_mov16Bytes :: Bool
prop_mov16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16
            src <- allocVar16
            mov16 dst src
    in  bytes == [0xA5, 0x04, 0x85, 0x02, 0xA5, 0x05, 0x85, 0x03]

prop_cmp16Bytes :: Bool
prop_cmp16Bytes =
    let bytes = asmZP $ do
            a <- allocVar16
            b <- allocVar16
            cmp16 a b
    in  bytes == [0xA5, 0x02, 0xC5, 0x04, 0xA5, 0x03, 0xE5, 0x05]

-- ---------------------------------------------------------------------------
-- Loc16 typeclass: Mem16 emits absolute, Ptr emits ZP (5 props)
-- ---------------------------------------------------------------------------

prop_lshift16Mem16 :: Bool
prop_lshift16Mem16 =
    -- lshift16 on Mem16 should emit ASL Absolute, ROL Absolute
    asm (lshift16 (Mem16 0xC600))
    == [ 0x0E, 0x00, 0xC6   -- ASL $C600
       , 0x2E, 0x01, 0xC6   -- ROL $C601
       ]

prop_rshift16Mem16 :: Bool
prop_rshift16Mem16 =
    -- rshift16 on Mem16 should emit LSR Absolute, ROR Absolute
    asm (rshift16 (Mem16 0xC650))
    == [ 0x4E, 0x51, 0xC6   -- LSR $C651
       , 0x6E, 0x50, 0xC6   -- ROR $C650
       ]

prop_load16Mem16 :: Bool
prop_load16Mem16 =
    -- load16 on Mem16 should emit LDA #imm, STA Absolute pairs
    asm (load16 (Mem16 0xC600) 0xABCD)
    == [ 0xA9, 0xCD, 0x8D, 0x00, 0xC6   -- LDA #$CD; STA $C600
       , 0xA9, 0xAB, 0x8D, 0x01, 0xC6   -- LDA #$AB; STA $C601
       ]

prop_lshift16Ptr :: Bool
prop_lshift16Ptr =
    -- lshift16 on Ptr should emit ASL ZeroPage, ROL ZeroPage
    asmZP (allocPtr >>= \p -> lshift16 p)
    == [ 0x06, 0x02   -- ASL $02
       , 0x26, 0x03   -- ROL $03
       ]

prop_inc16Mem16 :: Bool
prop_inc16Mem16 =
    -- inc16 on Mem16 should emit INC Absolute with BNE skip
    asm (inc16 (Mem16 0xC600))
    == [ 0xEE, 0x00, 0xC6   -- INC $C600
       , 0xD0, 0x03          -- BNE +3
       , 0xEE, 0x01, 0xC6   -- INC $C601
       ]

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "Control flow"
    , checkOnce "if_eq uses BNE"            prop_ifEqUsesBne
    , checkOnce "if_ne uses BEQ"            prop_ifNeUsesBeq
    , checkOnce "if_cs uses BCC"            prop_ifCsUsesBcc
    , checkOnce "if_cc uses BCS"            prop_ifCcUsesBcs
    , checkOnce "if_pl uses BMI"            prop_ifPlUsesBmi
    , checkOnce "if_mi uses BPL"            prop_ifMiUsesBpl
    , checkOnce "if_ larger then block"     prop_ifLargerThenBlock
    , checkOnce "if_ generalized (bvc)"     prop_ifGeneralized
    , checkOnce "for_x bytes"               prop_forXBytes
    , checkOnce "for_y bytes"               prop_forYBytes
    , checkOnce "for_x multi-byte body"     prop_forXMultiByteBody
    , checkOnce "loop_ bytes"               prop_loopBytes
    , checkOnce "while_ bytes"              prop_whileBytes
    , checkOnce "if_eq multi-byte bodies"   prop_ifEqMultiByteBodies
    , checkOnce "while_ multi-byte body"    prop_whileMultiByteBody

    , section "16-bit operations"
    , checkOnce "load16 bytes"              prop_load16Bytes
    , checkOnce "inc16 bytes"               prop_inc16Bytes
    , checkOnce "dec16 bytes"               prop_dec16Bytes
    , checkOnce "add16 bytes"               prop_add16Bytes
    , checkOnce "sub16 bytes"               prop_sub16Bytes
    , checkOnce "mov16 bytes"               prop_mov16Bytes
    , checkOnce "cmp16 bytes"               prop_cmp16Bytes

    , section "Loc16 typeclass"
    , checkOnce "lshift16 Mem16 (absolute)" prop_lshift16Mem16
    , checkOnce "rshift16 Mem16 (absolute)" prop_rshift16Mem16
    , checkOnce "load16 Mem16 (absolute)"   prop_load16Mem16
    , checkOnce "lshift16 Ptr (zero-page)"  prop_lshift16Ptr
    , checkOnce "inc16 Mem16 (absolute)"    prop_inc16Mem16
    ]
