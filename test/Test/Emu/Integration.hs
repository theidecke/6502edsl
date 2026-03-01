module Test.Emu.Integration (tests) where

import Data.Word (Word8, Word16)

import Test.Helpers (check, section, asm)
import Emu.CPU
import Emu.Trace (loadProgram, runN)
import Asm.Mos6502 (lda, sta, (#))
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)

tests :: [IO Bool]
tests =
    [ section "Emu.Integration (assembler → emulator)"
    , check "LDA/STA roundtrip"       prop_lda_sta
    , check "JSR/RTS flow"            prop_jsr_rts
    , check "BNE forward branch"      prop_bne_forward
    , check "INX loop"                prop_inx_loop
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
