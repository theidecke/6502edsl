module Test.Emu.Integration (tests) where

import Data.Word (Word8, Word16)

import Test.Helpers (check, section, asm, asmZP)
import Emu.CPU
import Emu.Trace (loadProgram, runN, runUntil, watch16)
import Asm.Mos6502 (allocVar16, lda, sta, (#))
import Asm.Mos6502.Ops16 (load16, inc16, dec16, add16, sub16, mov16, cmp16)
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)

tests :: [IO Bool]
tests =
    [ section "Emu.Integration (assembler → emulator)"
    , check "LDA/STA roundtrip"       prop_lda_sta
    , check "JSR/RTS flow"            prop_jsr_rts
    , check "BNE forward branch"      prop_bne_forward
    , check "INX loop"                prop_inx_loop
    , section "Emu.Integration (16-bit operations)"
    , check "load16"                  prop_load16
    , check "inc16"                   prop_inc16
    , check "dec16"                   prop_dec16
    , check "add16"                   prop_add16
    , check "sub16"                   prop_sub16
    , check "mov16"                   prop_mov16
    , check "cmp16"                   prop_cmp16
    ]

-- | Assemble, load into emulator, run, check result.
asmRun :: Word16 -> [Word8] -> Int -> CPUState
asmRun org bs n = runN n (loadProgram org bs initCPU)

prop_lda_sta :: Bool
prop_lda_sta =
    let bs = asm $ do
            lda # (0x42 :: Word8)
            sta (0x0200 :: Word16)
        s = asmRun 0x0000 bs 2
    in  view regA s == 0x42 && view (memAt 0x0200) s == 0x42

prop_jsr_rts :: Bool
prop_jsr_rts =
    -- Assemble JSR + halt (JMP self) at $0800, subroutine at $0810
    -- Subroutine: LDA #$99, RTS
    let sub_bytes = encode (Instruction LDA (Immediate 0x99))
                 ++ encode (Instruction RTS Implied)
        -- JSR $0810 + JMP $0803 (halt)
        main_bytes = encode (Instruction JSR (Absolute 0x0810))
                  ++ encode (Instruction JMP (Absolute 0x0803))
        s0 = loadProgram 0x0800 main_bytes $
             loadProgram 0x0810 sub_bytes initCPU
        -- Need to set PC to main
        s1 = set regPC 0x0800 s0
        -- Run: JSR (1) + LDA (2) + RTS (3) + JMP (4)
        s2 = runN 4 s1
    in  view regA s2 == 0x99 && view regPC s2 == 0x0803

prop_bne_forward :: Bool
prop_bne_forward =
    -- LDA #$01; BNE +2; LDA #$FF; NOP
    -- If BNE works, A should still be $01 and we skip the LDA #$FF
    let bs = encode (Instruction LDA (Immediate 0x01))  -- 2 bytes
          ++ encode (Instruction BNE (Relative 2))       -- 2 bytes, skips next 2
          ++ encode (Instruction LDA (Immediate 0xFF))   -- 2 bytes (skipped)
          ++ encode (Instruction NOP Implied)             -- 1 byte
        s = asmRun 0x0000 bs 3  -- LDA, BNE (taken), NOP
    in  view regA s == 0x01

prop_inx_loop :: Bool
prop_inx_loop =
    -- INX 3 times from X=0
    let bs = encode (Instruction INX Implied)
          ++ encode (Instruction INX Implied)
          ++ encode (Instruction INX Implied)
        s = asmRun 0x0000 bs 3
    in  view regX s == 3

-- ---------------------------------------------------------------------------
-- 16-bit operation properties (emulator-verified)
-- ---------------------------------------------------------------------------

-- | Load assembled program at $0800, run until PC passes the end.
runProg :: [Word8] -> CPUState -> CPUState
runProg bs = runUntil done . loadProgram 0x0800 bs
  where done s = view regPC s >= 0x0800 + fromIntegral (length bs)

prop_load16 :: Word16 -> Bool
prop_load16 val =
    let bs = asmZP $ do v <- allocVar16; load16 v val
        s  = runProg bs initCPU
    in  watch16 0x02 s == val

prop_inc16 :: Word16 -> Bool
prop_inc16 val =
    let bs = asmZP $ do v <- allocVar16; load16 v val; inc16 v
        s  = runProg bs initCPU
    in  watch16 0x02 s == val + 1

prop_dec16 :: Word16 -> Bool
prop_dec16 val =
    let bs = asmZP $ do v <- allocVar16; load16 v val; dec16 v
        s  = runProg bs initCPU
    in  watch16 0x02 s == val - 1

prop_add16 :: Word16 -> Word16 -> Bool
prop_add16 a b =
    let bs = asmZP $ do
            dst <- allocVar16
            va  <- allocVar16
            vb  <- allocVar16
            load16 va a
            load16 vb b
            add16 dst va vb
        s = runProg bs initCPU
    in  watch16 0x02 s == a + b
     && view flagC s == (toInteger a + toInteger b > 0xFFFF)

prop_sub16 :: Word16 -> Word16 -> Bool
prop_sub16 a b =
    let bs = asmZP $ do
            dst <- allocVar16
            va  <- allocVar16
            vb  <- allocVar16
            load16 va a
            load16 vb b
            sub16 dst va vb
        s = runProg bs initCPU
    in  watch16 0x02 s == a - b
     && view flagC s == (a >= b)

prop_mov16 :: Word16 -> Bool
prop_mov16 val =
    let bs = asmZP $ do
            dst <- allocVar16
            src <- allocVar16
            load16 src val
            mov16 dst src
        s = runProg bs initCPU
    in  watch16 0x02 s == val

prop_cmp16 :: Word16 -> Word16 -> Bool
prop_cmp16 a b =
    let bs = asmZP $ do
            va <- allocVar16
            vb <- allocVar16
            load16 va a
            load16 vb b
            cmp16 va vb
        s = runProg bs initCPU
    in  view flagC s == (a >= b)
