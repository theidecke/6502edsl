module Test.Emu.Step (tests) where

import Data.Bits ((.&.), (.|.), xor, testBit, shiftL, shiftR, complement)
import Data.Word (Word8, Word16)
import Test.Helpers (check, section)
import ISA.Mos6502
    ( Opcode(..), AddressingMode(..), Instruction(..)
    , baseCycles
    )
import Emu.CPU
import Emu.Step (execute)

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Execute a single instruction from a given CPU state.
exec :: Instruction -> CPUState -> CPUState
exec = execute

-- | Store a value at an address and return modified state
withMem :: Word16 -> Word8 -> CPUState -> CPUState
withMem addr val = set (memAt addr) val

tests :: [IO Bool]
tests =
    [ section "Emu.Step — Load/Store"
    , check "LDA imm sets A and N/Z"       prop_lda_imm
    , check "LDA zp reads memory"           prop_lda_zp
    , check "LDA abs reads memory"          prop_lda_abs
    , check "LDX imm sets X and N/Z"       prop_ldx_imm
    , check "LDY imm sets Y and N/Z"       prop_ldy_imm
    , check "STA stores A without flags"    prop_sta_zp
    , check "STX stores X without flags"    prop_stx_zp
    , check "STY stores Y without flags"    prop_sty_zp

    , section "Emu.Step — Transfer"
    , check "TAX transfers A to X, updates N/Z" prop_tax
    , check "TAY transfers A to Y, updates N/Z" prop_tay
    , check "TXA transfers X to A, updates N/Z" prop_txa
    , check "TYA transfers Y to A, updates N/Z" prop_tya
    , check "TSX transfers SP to X, updates N/Z" prop_tsx
    , check "TXS transfers X to SP, no N/Z" prop_txs

    , section "Emu.Step — Stack"
    , check "PHA/PLA roundtrip"             prop_pha_pla
    , check "PHP pushes P with B+bit5"      prop_php
    , check "PLP clears B, sets bit5"       prop_plp
    , check "SP wraps in page 1"            prop_sp_wrap

    , section "Emu.Step — ADC/SBC binary"
    , check "ADC binary known sums"         prop_adc_bin
    , check "ADC binary carry out"          prop_adc_carry_out
    , check "ADC binary overflow V"         prop_adc_overflow
    , check "SBC binary known diffs"        prop_sbc_bin
    , check "SBC binary borrow"             prop_sbc_borrow
    , check "SBC binary overflow V"         prop_sbc_overflow

    , section "Emu.Step — ADC/SBC decimal"
    , check "ADC decimal 0x15+0x27=0x42"    prop_adc_bcd_1
    , check "ADC decimal 0x99+0x01=0x00,C=1" prop_adc_bcd_2
    , check "SBC decimal 0x42-0x15=0x27"    prop_sbc_bcd_1

    , section "Emu.Step — Logic"
    , check "AND"                           prop_and
    , check "ORA"                           prop_ora
    , check "EOR"                           prop_eor

    , section "Emu.Step — Compare"
    , check "CMP equal"                     prop_cmp_eq
    , check "CMP greater"                   prop_cmp_gt
    , check "CMP less"                      prop_cmp_lt
    , check "CPX"                           prop_cpx
    , check "CPY"                           prop_cpy

    , section "Emu.Step — Shift/Rotate"
    , check "ASL accumulator"               prop_asl_a
    , check "LSR accumulator"               prop_lsr_a
    , check "ROL accumulator"               prop_rol_a
    , check "ROR accumulator"               prop_ror_a
    , check "ASL memory"                    prop_asl_mem
    , check "LSR memory"                    prop_lsr_mem

    , section "Emu.Step — Inc/Dec"
    , check "INC memory"                    prop_inc_mem
    , check "DEC memory"                    prop_dec_mem
    , check "INX wraps"                     prop_inx
    , check "DEX wraps"                     prop_dex
    , check "INY wraps"                     prop_iny
    , check "DEY wraps"                     prop_dey

    , section "Emu.Step — Branch"
    , check "BEQ taken"                     prop_beq_taken
    , check "BEQ not taken"                 prop_beq_not_taken
    , check "BNE taken"                     prop_bne_taken
    , check "BCC/BCS"                       prop_bcc_bcs
    , check "BMI/BPL"                       prop_bmi_bpl
    , check "BVS/BVC"                       prop_bvs_bvc
    , check "Branch backward"               prop_branch_backward
    , check "Branch page cross cycle"       prop_branch_page_cross

    , section "Emu.Step — Jump"
    , check "JMP abs"                       prop_jmp_abs
    , check "JMP indirect"                  prop_jmp_indirect
    , check "JMP indirect page bug"         prop_jmp_indirect_bug
    , check "JSR/RTS roundtrip"             prop_jsr_rts
    , check "RTI pulls P then PC"           prop_rti

    , section "Emu.Step — Flags"
    , check "CLC/SEC"                       prop_clc_sec
    , check "CLD/SED"                       prop_cld_sed
    , check "CLI/SEI"                       prop_cli_sei
    , check "CLV"                           prop_clv

    , section "Emu.Step — BIT"
    , check "BIT sets N/V from memory, Z from A&M" prop_bit

    , section "Emu.Step — BRK"
    , check "BRK pushes PC+2, P|B, reads vector" prop_brk

    , section "Emu.Step — NOP"
    , check "NOP only changes PC and cycles" prop_nop

    , section "Emu.Step — Cycle counting"
    , check "Cycles match baseCycles for non-crossing" prop_cycles_base
    , check "Page-cross penalty for AbsoluteX read" prop_page_cross_penalty
    ]

-- ---------------------------------------------------------------------------
-- Load/Store
-- ---------------------------------------------------------------------------

prop_lda_imm :: Word8 -> Bool
prop_lda_imm val =
    let s = exec (Instruction LDA (Immediate val)) initCPU
    in  view regA s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_lda_zp :: Word8 -> Word8 -> Bool
prop_lda_zp addr val =
    let s0 = withMem (fromIntegral addr) val initCPU
        s  = exec (Instruction LDA (ZeroPage addr)) s0
    in  view regA s == val

prop_lda_abs :: Word16 -> Word8 -> Bool
prop_lda_abs addr val =
    let s0 = withMem addr val initCPU
        s  = exec (Instruction LDA (Absolute addr)) s0
    in  view regA s == val

prop_ldx_imm :: Word8 -> Bool
prop_ldx_imm val =
    let s = exec (Instruction LDX (Immediate val)) initCPU
    in  view regX s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_ldy_imm :: Word8 -> Bool
prop_ldy_imm val =
    let s = exec (Instruction LDY (Immediate val)) initCPU
    in  view regY s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_sta_zp :: Word8 -> Word8 -> Bool
prop_sta_zp addr val =
    let s0 = set regA val initCPU
        s  = exec (Instruction STA (ZeroPage addr)) s0
    in  view (memAt (fromIntegral addr)) s == val &&
        view regP s == view regP s0  -- flags unchanged (just compare P)

prop_stx_zp :: Word8 -> Word8 -> Bool
prop_stx_zp addr val =
    let s0 = set regX val initCPU
        s  = exec (Instruction STX (ZeroPage addr)) s0
    in  view (memAt (fromIntegral addr)) s == val &&
        view regP s == view regP s0

prop_sty_zp :: Word8 -> Word8 -> Bool
prop_sty_zp addr val =
    let s0 = set regY val initCPU
        s  = exec (Instruction STY (ZeroPage addr)) s0
    in  view (memAt (fromIntegral addr)) s == val &&
        view regP s == view regP s0

-- ---------------------------------------------------------------------------
-- Transfer
-- ---------------------------------------------------------------------------

prop_tax :: Word8 -> Bool
prop_tax val =
    let s = exec (Instruction TAX Implied) (set regA val initCPU)
    in  view regX s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_tay :: Word8 -> Bool
prop_tay val =
    let s = exec (Instruction TAY Implied) (set regA val initCPU)
    in  view regY s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_txa :: Word8 -> Bool
prop_txa val =
    let s = exec (Instruction TXA Implied) (set regX val initCPU)
    in  view regA s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_tya :: Word8 -> Bool
prop_tya val =
    let s = exec (Instruction TYA Implied) (set regY val initCPU)
    in  view regA s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_tsx :: Word8 -> Bool
prop_tsx val =
    let s = exec (Instruction TSX Implied) (set regSP val initCPU)
    in  view regX s == val && view flagZ s == (val == 0) && view flagN s == testBit val 7

prop_txs :: Word8 -> Bool
prop_txs val =
    let s0 = set regX val $ set regP 0x00 initCPU  -- clear all flags
        s  = exec (Instruction TXS Implied) s0
    in  view regSP s == val && view regP s == view regP s0  -- no flag changes

-- ---------------------------------------------------------------------------
-- Stack
-- ---------------------------------------------------------------------------

prop_pha_pla :: Word8 -> Bool
prop_pha_pla val =
    let s0 = set regA val initCPU
        s1 = exec (Instruction PHA Implied) s0
        s2 = exec (Instruction LDA (Immediate 0)) s1  -- clear A
        s3 = exec (Instruction PLA Implied) s2
    in  view regA s3 == val

prop_php :: Bool
prop_php =
    let s0 = set regP 0xC3 initCPU  -- N=1, V=1, C=1, Z=1
        s1 = exec (Instruction PHP Implied) s0
        sp = view regSP s1
        pushed = view (memAt (0x0100 + fromIntegral (sp + 1))) s1
    in  pushed == (0xC3 .|. 0x30)  -- B flag + bit5 forced set

prop_plp :: Bool
prop_plp =
    let -- Push 0xFF onto stack manually
        s0 = set regSP 0xFC initCPU
        s1 = set (memAt 0x01FD) 0xFF s0
        s2 = exec (Instruction PLP Implied) s1
    in  view regP s2 == (0xFF .|. 0x20) .&. complement 0x10  -- bit5 set, B clear = 0xEF

prop_sp_wrap :: Bool
prop_sp_wrap =
    let s0 = set regSP 0x00 initCPU  -- SP at bottom
        s1 = exec (Instruction PHA Implied) s0  -- push wraps SP to 0xFF
    in  view regSP s1 == 0xFF

-- ---------------------------------------------------------------------------
-- ADC / SBC binary
-- ---------------------------------------------------------------------------

prop_adc_bin :: Bool
prop_adc_bin =
    let s0 = set regA 0x10 $ set flagC False $ set flagD False initCPU
        s1 = exec (Instruction ADC (Immediate 0x20)) s0
    in  view regA s1 == 0x30 && not (view flagC s1) && not (view flagV s1)

prop_adc_carry_out :: Bool
prop_adc_carry_out =
    let s0 = set regA 0xFF $ set flagC False $ set flagD False initCPU
        s1 = exec (Instruction ADC (Immediate 0x01)) s0
    in  view regA s1 == 0x00 && view flagC s1 && view flagZ s1

prop_adc_overflow :: Bool
prop_adc_overflow =
    let -- 0x50 + 0x50 = 0xA0 — positive + positive = negative → overflow
        s0 = set regA 0x50 $ set flagC False $ set flagD False initCPU
        s1 = exec (Instruction ADC (Immediate 0x50)) s0
    in  view regA s1 == 0xA0 && view flagV s1 && view flagN s1

prop_sbc_bin :: Bool
prop_sbc_bin =
    let s0 = set regA 0x30 $ set flagC True $ set flagD False initCPU
        s1 = exec (Instruction SBC (Immediate 0x10)) s0
    in  view regA s1 == 0x20 && view flagC s1

prop_sbc_borrow :: Bool
prop_sbc_borrow =
    let s0 = set regA 0x00 $ set flagC True $ set flagD False initCPU
        s1 = exec (Instruction SBC (Immediate 0x01)) s0
    in  view regA s1 == 0xFF && not (view flagC s1)

prop_sbc_overflow :: Bool
prop_sbc_overflow =
    let -- 0x50 - 0xB0 (with C=1): 0x50 - 0xB0 = 0xA0, positive - negative = negative → overflow
        s0 = set regA 0x50 $ set flagC True $ set flagD False initCPU
        s1 = exec (Instruction SBC (Immediate 0xB0)) s0
    in  view regA s1 == 0xA0 && view flagV s1

-- ---------------------------------------------------------------------------
-- ADC / SBC decimal
-- ---------------------------------------------------------------------------

prop_adc_bcd_1 :: Bool
prop_adc_bcd_1 =
    let s0 = set regA 0x15 $ set flagC False $ set flagD True initCPU
        s1 = exec (Instruction ADC (Immediate 0x27)) s0
    in  view regA s1 == 0x42 && not (view flagC s1)

prop_adc_bcd_2 :: Bool
prop_adc_bcd_2 =
    let s0 = set regA 0x99 $ set flagC False $ set flagD True initCPU
        s1 = exec (Instruction ADC (Immediate 0x01)) s0
    in  view regA s1 == 0x00 && view flagC s1

prop_sbc_bcd_1 :: Bool
prop_sbc_bcd_1 =
    let s0 = set regA 0x42 $ set flagC True $ set flagD True initCPU
        s1 = exec (Instruction SBC (Immediate 0x15)) s0
    in  view regA s1 == 0x27 && view flagC s1

-- ---------------------------------------------------------------------------
-- Logic
-- ---------------------------------------------------------------------------

prop_and :: Word8 -> Word8 -> Bool
prop_and a m =
    let s = exec (Instruction AND (Immediate m)) (set regA a $ set flagD False initCPU)
        expected = a .&. m
    in  view regA s == expected &&
        view flagZ s == (expected == 0) &&
        view flagN s == testBit expected 7

prop_ora :: Word8 -> Word8 -> Bool
prop_ora a m =
    let s = exec (Instruction ORA (Immediate m)) (set regA a $ set flagD False initCPU)
        expected = a .|. m
    in  view regA s == expected &&
        view flagZ s == (expected == 0) &&
        view flagN s == testBit expected 7

prop_eor :: Word8 -> Word8 -> Bool
prop_eor a m =
    let s = exec (Instruction EOR (Immediate m)) (set regA a $ set flagD False initCPU)
        expected = a `xor` m
    in  view regA s == expected &&
        view flagZ s == (expected == 0) &&
        view flagN s == testBit expected 7

-- ---------------------------------------------------------------------------
-- Compare
-- ---------------------------------------------------------------------------

prop_cmp_eq :: Word8 -> Bool
prop_cmp_eq val =
    let s = exec (Instruction CMP (Immediate val)) (set regA val initCPU)
    in  view flagZ s && view flagC s

prop_cmp_gt :: Bool
prop_cmp_gt =
    let s = exec (Instruction CMP (Immediate 0x10)) (set regA 0x50 initCPU)
    in  not (view flagZ s) && view flagC s

prop_cmp_lt :: Bool
prop_cmp_lt =
    let s = exec (Instruction CMP (Immediate 0x50)) (set regA 0x10 initCPU)
    in  not (view flagZ s) && not (view flagC s)

prop_cpx :: Word8 -> Bool
prop_cpx val =
    let sEq = exec (Instruction CPX (Immediate val)) (set regX val initCPU)
        sGt = exec (Instruction CPX (Immediate 0x10)) (set regX 0x50 initCPU)
        sLt = exec (Instruction CPX (Immediate 0x50)) (set regX 0x10 initCPU)
    in  view flagZ sEq && view flagC sEq &&
        not (view flagZ sGt) && view flagC sGt &&
        not (view flagZ sLt) && not (view flagC sLt)

prop_cpy :: Word8 -> Bool
prop_cpy val =
    let sEq = exec (Instruction CPY (Immediate val)) (set regY val initCPU)
        sGt = exec (Instruction CPY (Immediate 0x10)) (set regY 0x50 initCPU)
        sLt = exec (Instruction CPY (Immediate 0x50)) (set regY 0x10 initCPU)
    in  view flagZ sEq && view flagC sEq &&
        not (view flagZ sGt) && view flagC sGt &&
        not (view flagZ sLt) && not (view flagC sLt)

-- ---------------------------------------------------------------------------
-- Shift / Rotate
-- ---------------------------------------------------------------------------

prop_asl_a :: Word8 -> Bool
prop_asl_a val =
    let s = exec (Instruction ASL Accumulator) (set regA val initCPU)
        expected = val `shiftL` 1
    in  view regA s == expected && view flagC s == testBit val 7

prop_lsr_a :: Word8 -> Bool
prop_lsr_a val =
    let s = exec (Instruction LSR Accumulator) (set regA val initCPU)
        expected = val `shiftR` 1
    in  view regA s == expected && view flagC s == testBit val 0

prop_rol_a :: Word8 -> Bool -> Bool
prop_rol_a val cin =
    let s0 = set regA val $ set flagC cin initCPU
        s = exec (Instruction ROL Accumulator) s0
        expected = (val `shiftL` 1) .|. (if cin then 1 else 0)
    in  view regA s == expected && view flagC s == testBit val 7

prop_ror_a :: Word8 -> Bool -> Bool
prop_ror_a val cin =
    let s0 = set regA val $ set flagC cin initCPU
        s = exec (Instruction ROR Accumulator) s0
        expected = (val `shiftR` 1) .|. (if cin then 0x80 else 0)
    in  view regA s == expected && view flagC s == testBit val 0

prop_asl_mem :: Word8 -> Word8 -> Bool
prop_asl_mem addr val =
    let s0 = withMem (fromIntegral addr) val initCPU
        s  = exec (Instruction ASL (ZeroPage addr)) s0
    in  view (memAt (fromIntegral addr)) s == (val `shiftL` 1) &&
        view flagC s == testBit val 7

prop_lsr_mem :: Word8 -> Word8 -> Bool
prop_lsr_mem addr val =
    let s0 = withMem (fromIntegral addr) val initCPU
        s  = exec (Instruction LSR (ZeroPage addr)) s0
    in  view (memAt (fromIntegral addr)) s == (val `shiftR` 1) &&
        view flagC s == testBit val 0

-- ---------------------------------------------------------------------------
-- Inc / Dec
-- ---------------------------------------------------------------------------

prop_inc_mem :: Word8 -> Word8 -> Bool
prop_inc_mem addr val =
    let s0 = withMem (fromIntegral addr) val initCPU
        s  = exec (Instruction INC (ZeroPage addr)) s0
        res = val + 1
    in  view (memAt (fromIntegral addr)) s == res &&
        view flagZ s == (res == 0) &&
        view flagN s == testBit res 7

prop_dec_mem :: Word8 -> Word8 -> Bool
prop_dec_mem addr val =
    let s0 = withMem (fromIntegral addr) val initCPU
        s  = exec (Instruction DEC (ZeroPage addr)) s0
        res = val - 1
    in  view (memAt (fromIntegral addr)) s == res &&
        view flagZ s == (res == 0) &&
        view flagN s == testBit res 7

prop_inx :: Bool
prop_inx =
    let s = exec (Instruction INX Implied) (set regX 0xFF initCPU)
    in  view regX s == 0x00 && view flagZ s

prop_dex :: Bool
prop_dex =
    let s = exec (Instruction DEX Implied) (set regX 0x00 initCPU)
    in  view regX s == 0xFF && view flagN s

prop_iny :: Bool
prop_iny =
    let s = exec (Instruction INY Implied) (set regY 0xFF initCPU)
    in  view regY s == 0x00 && view flagZ s

prop_dey :: Bool
prop_dey =
    let s = exec (Instruction DEY Implied) (set regY 0x00 initCPU)
    in  view regY s == 0xFF && view flagN s

-- ---------------------------------------------------------------------------
-- Branches
-- ---------------------------------------------------------------------------

prop_beq_taken :: Bool
prop_beq_taken =
    let s0 = set flagZ True $ set regPC 0x1000 initCPU
        s  = exec (Instruction BEQ (Relative 0x10)) s0
    in  view regPC s == 0x1012  -- PC+2 + offset

prop_beq_not_taken :: Bool
prop_beq_not_taken =
    let s0 = set flagZ False $ set regPC 0x1000 initCPU
        s  = exec (Instruction BEQ (Relative 0x10)) s0
    in  view regPC s == 0x1002  -- PC+2 only

prop_bne_taken :: Bool
prop_bne_taken =
    let s0 = set flagZ False $ set regPC 0x1000 initCPU
        s  = exec (Instruction BNE (Relative 0x05)) s0
    in  view regPC s == 0x1007

prop_bcc_bcs :: Bool
prop_bcc_bcs =
    let s_clr = set flagC False $ set regPC 0x1000 initCPU
        s_set = set flagC True  $ set regPC 0x1000 initCPU
    in  view regPC (exec (Instruction BCC (Relative 0x05)) s_clr) == 0x1007 &&
        view regPC (exec (Instruction BCS (Relative 0x05)) s_set) == 0x1007

prop_bmi_bpl :: Bool
prop_bmi_bpl =
    let s_neg = set flagN True  $ set regPC 0x1000 initCPU
        s_pos = set flagN False $ set regPC 0x1000 initCPU
    in  view regPC (exec (Instruction BMI (Relative 0x05)) s_neg) == 0x1007 &&
        view regPC (exec (Instruction BPL (Relative 0x05)) s_pos) == 0x1007

prop_bvs_bvc :: Bool
prop_bvs_bvc =
    let s_set = set flagV True  $ set regPC 0x1000 initCPU
        s_clr = set flagV False $ set regPC 0x1000 initCPU
    in  view regPC (exec (Instruction BVS (Relative 0x05)) s_set) == 0x1007 &&
        view regPC (exec (Instruction BVC (Relative 0x05)) s_clr) == 0x1007

prop_branch_backward :: Bool
prop_branch_backward =
    let s0 = set flagZ True $ set regPC 0x1010 initCPU
        s  = exec (Instruction BEQ (Relative (-16))) s0 -- 0xF0 = -16
    in  view regPC s == 0x1002  -- 0x1012 + (-16) = 0x1002

prop_branch_page_cross :: Bool
prop_branch_page_cross =
    let s0 = set flagZ True $ set regPC 0x10F0 initCPU
        s  = exec (Instruction BEQ (Relative 0x20)) s0
        -- PC after fetch = 0x10F2, target = 0x10F2 + 0x20 = 0x1112 (page cross)
        -- Cycles = base(2) + taken(1) + page_cross(1) = 4
    in  view regPC s == 0x1112 && view cycles s == 4

-- ---------------------------------------------------------------------------
-- Jump
-- ---------------------------------------------------------------------------

prop_jmp_abs :: Bool
prop_jmp_abs =
    let s = exec (Instruction JMP (Absolute 0xC000)) initCPU
    in  view regPC s == 0xC000

prop_jmp_indirect :: Bool
prop_jmp_indirect =
    let s0 = withMem 0x1000 0x00 $ withMem 0x1001 0xC0 initCPU
        s  = exec (Instruction JMP (Indirect 0x1000)) s0
    in  view regPC s == 0xC000

prop_jmp_indirect_bug :: Bool
prop_jmp_indirect_bug =
    -- 6502 page boundary bug: JMP ($10FF) reads lo from $10FF and hi from $1000
    let s0 = withMem 0x10FF 0x34 $ withMem 0x1000 0x12 initCPU
        s  = exec (Instruction JMP (Indirect 0x10FF)) s0
    in  view regPC s == 0x1234

prop_jsr_rts :: Bool
prop_jsr_rts =
    let s0 = set regPC 0x1000 initCPU
        -- JSR pushes (PC+3-1) = 0x1002 onto stack, sets PC to target
        s1 = exec (Instruction JSR (Absolute 0x2000)) s0
        s2 = exec (Instruction RTS Implied) s1
    in  view regPC s1 == 0x2000 && view regPC s2 == 0x1003

prop_rti :: Bool
prop_rti =
    let -- Set up stack: push PC=0x1234 then P=0xC3
        s0 = set regSP 0xFA initCPU
        s1 = set (memAt 0x01FB) 0xC3 s0     -- P
        s2 = set (memAt 0x01FC) 0x34 s1     -- PC lo
        s3 = set (memAt 0x01FD) 0x12 s2     -- PC hi
        s4 = exec (Instruction RTI Implied) s3
    in  view regPC s4 == 0x1234 &&
        view regP s4 == ((0xC3 .|. 0x20) .&. complement 0x10)  -- bit5 set, B clear

-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

prop_clc_sec :: Bool
prop_clc_sec =
    let s1 = exec (Instruction SEC Implied) initCPU
        s2 = exec (Instruction CLC Implied) s1
    in  view flagC s1 && not (view flagC s2)

prop_cld_sed :: Bool
prop_cld_sed =
    let s1 = exec (Instruction SED Implied) initCPU
        s2 = exec (Instruction CLD Implied) s1
    in  view flagD s1 && not (view flagD s2)

prop_cli_sei :: Bool
prop_cli_sei =
    let s0 = set flagI False initCPU
        s1 = exec (Instruction SEI Implied) s0
        s2 = exec (Instruction CLI Implied) s1
    in  view flagI s1 && not (view flagI s2)

prop_clv :: Bool
prop_clv =
    let s0 = set flagV True initCPU
        s1 = exec (Instruction CLV Implied) s0
    in  not (view flagV s1)

-- ---------------------------------------------------------------------------
-- BIT
-- ---------------------------------------------------------------------------

prop_bit :: Word8 -> Word8 -> Bool
prop_bit a m =
    let s0 = set regA a $ withMem 0x0010 m initCPU
        s  = exec (Instruction BIT (ZeroPage 0x10)) s0
    in  view flagZ s == ((a .&. m) == 0) &&
        view flagV s == testBit m 6 &&
        view flagN s == testBit m 7

-- ---------------------------------------------------------------------------
-- BRK
-- ---------------------------------------------------------------------------

prop_brk :: Bool
prop_brk =
    let s0 = set regPC 0x1000 $ set regP 0x00 $
             withMem 0xFFFE 0x00 $ withMem 0xFFFF 0x80 initCPU
        s1 = exec (Instruction BRK Implied) s0
        -- Check pushed values on stack
        sp = view regSP s1
    in  view regPC s1 == 0x8000 &&          -- loaded from IRQ vector
        view flagI s1 &&                     -- I flag set
        view (memAt (0x0100 + fromIntegral (sp + 3))) s1 == 0x10 &&  -- PC hi
        view (memAt (0x0100 + fromIntegral (sp + 2))) s1 == 0x02 &&  -- PC lo (PC+2)
        (view (memAt (0x0100 + fromIntegral (sp + 1))) s1 .&. 0x30) == 0x30  -- B+bit5

-- ---------------------------------------------------------------------------
-- NOP
-- ---------------------------------------------------------------------------

prop_nop :: Bool
prop_nop =
    let s0 = set regPC 0x1000 initCPU
        s  = exec (Instruction NOP Implied) s0
    in  view regA s == 0 && view regX s == 0 && view regY s == 0 &&
        view regPC s == 0x1001 && view cycles s == 2

-- ---------------------------------------------------------------------------
-- Cycle counting
-- ---------------------------------------------------------------------------

prop_cycles_base :: Bool
prop_cycles_base =
    let instr = Instruction LDA (Immediate 0x42)
        s = exec instr initCPU
    in  view cycles s == baseCycles instr

prop_page_cross_penalty :: Bool
prop_page_cross_penalty =
    -- LDA $10FF,X with X=1 causes page cross ($10FF + 1 = $1100)
    let s0 = set regX 1 initCPU
        instr = Instruction LDA (AbsoluteX 0x10FF)
        s = exec instr s0
    in  view cycles s == baseCycles instr + 1  -- +1 for page cross
