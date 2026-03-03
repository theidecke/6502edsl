{-# LANGUAGE RecursiveDo #-}

module Test.Emu.Laziness (tests) where

import Data.Word (Word8, Word16)

import Test.Helpers (checkOnce, section, asmAt)
import Asm.Monad (label)
import Asm.Mos6502 (lda, jmp, nop, (#))
import Emu.CPU (CPUState(..), initCPU, view, regA)
import Emu.Mem (loadBytes, readByte)
import Emu.Step (step)
import Emu.Trace (trace, runN, loadProgram)
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)

tests :: [IO Bool]
tests =
    [ section "Emu.Laziness (end-to-end laziness)"
    , checkOnce "loadBytes preserves lazy values"       prop_loadBytesLazy
    , checkOnce "step ignores unrelated memory"          prop_stepIgnoresUnrelated
    , checkOnce "trace prefix is lazy"                   prop_tracePrefixLazy
    , checkOnce "unreachable JMP with undefined operand" prop_unreachableJmpUndefined
    , checkOnce "unused ZP containing undefined"         prop_unusedZpUndefined
    , checkOnce "mdo unreachable label (full pipeline)"  prop_mdoUnreachableLabel
    ]

-- | loadBytes with an undefined byte; reading only other addresses doesn't crash.
prop_loadBytesLazy :: Bool
prop_loadBytesLazy =
    let m = loadBytes 0x0800 [0x42, undefined, 0x99] (cpuMem initCPU)
    in  readByte 0x0800 m == 0x42 && readByte 0x0802 m == 0x99

-- | Execute NOP at $0800 with undefined bytes at $0900; never touches them.
prop_stepIgnoresUnrelated :: Bool
prop_stepIgnoresUnrelated =
    let s0 = loadProgram 0x0800 (encode (Instruction NOP Implied)) initCPU
        s1 = s0 { cpuMem = loadBytes 0x0900 [undefined] (cpuMem s0) }
        s2 = step s1
    in  view regA s2 == 0  -- NOP doesn't crash; A still 0

-- | 3 NOPs + undefined 4th byte; take 4 (trace s) succeeds (never fetches byte 4).
prop_tracePrefixLazy :: Bool
prop_tracePrefixLazy =
    let nopByte = 0xEA  -- NOP opcode
        s0 = loadProgram 0x0800 [nopByte, nopByte, nopByte, undefined] initCPU
        states = take 4 (trace s0)  -- initial + 3 steps (3 NOPs)
    in  length states == 4

-- | JMP skips dead code containing undefined operand bytes.
-- $0800: JMP $0806       [4C 06 08]
-- $0803: JMP <undefined> [4C ⊥  ⊥ ]
-- $0806: LDA #$42        [A9 42]
-- $0808: JMP $0808       [4C 08 08]  (halt)
prop_unreachableJmpUndefined :: Bool
prop_unreachableJmpUndefined =
    let jmpOver  = encode (Instruction JMP (Absolute 0x0806))
        deadJmp  = [0x4C, undefined, undefined :: Word8]
        ldaImm   = encode (Instruction LDA (Immediate 0x42))
        halt     = encode (Instruction JMP (Absolute 0x0808))
        program  = jmpOver ++ deadJmp ++ ldaImm ++ halt
        s = runN 3 (loadProgram 0x0800 program initCPU)
    in  view regA s == 0x42

-- | ZP address $02 contains undefined; program that never reads it runs fine.
prop_unusedZpUndefined :: Bool
prop_unusedZpUndefined =
    let program = encode (Instruction LDA (Immediate 0x42))
               ++ encode (Instruction NOP Implied)
        s0 = loadProgram 0x0800 program initCPU
        s1 = s0 { cpuMem = loadBytes 0x02 [undefined] (cpuMem s0) }
        s2 = runN 2 s1
    in  view regA s2 == 0x42

-- | Full pipeline: assemble with mdo, unreachable jmp to undefined label.
-- mdo
--     lda # 0x42
--     jmp done
--     jmp nowhere           -- dead code: operand bytes are undefined
--     nowhere <- pure undefined
--     done <- label
--     nop
prop_mdoUnreachableLabel :: Bool
prop_mdoUnreachableLabel =
    let bs = asmAt 0x0800 $ mdo
                lda # (0x42 :: Word8)
                jmp done
                jmp nowhere
                nowhere <- pure (undefined :: Word16)
                done <- label
                nop
        s = runN 3 (loadProgram 0x0800 bs initCPU)
    in  view regA s == 0x42
