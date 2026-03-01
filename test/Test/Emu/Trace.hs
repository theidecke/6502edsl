module Test.Emu.Trace (tests) where

import Data.Word (Word8)
import Test.QuickCheck ()

import Test.Helpers (check, section)
import Emu.CPU
import Emu.Trace

tests :: [IO Bool]
tests =
    [ section "Emu.Trace"
    , check "head of trace is initial state" prop_traceHead
    , check "runUntil finds JMP-to-self"     prop_runUntilJmpSelf
    , check "runN steps correct count"       prop_runN
    , check "loadProgram sets PC and memory" prop_loadProgram
    ]

prop_traceHead :: Bool
prop_traceHead =
    let s = initCPU
    in  case trace s of
            (x:_) -> x == s
            _     -> False

prop_runUntilJmpSelf :: Bool
prop_runUntilJmpSelf =
    -- Load "LDA #$42; JMP $0802" at $0800: A9 42 4C 02 08
    let s0 = loadProgram 0x0800 [0xA9, 0x42, 0x4C, 0x02, 0x08] initCPU
        s  = runUntil (\cpu -> view regPC cpu == 0x0802 && view regA cpu == 0x42) s0
    in  view regPC s == 0x0802 && view regA s == 0x42

prop_runN :: Bool
prop_runN =
    -- Three NOPs at $0800: EA EA EA
    let s0 = loadProgram 0x0800 [0xEA, 0xEA, 0xEA] initCPU
        s3 = runN 3 s0
    in  view regPC s3 == 0x0803 && view cycles s3 == 6

prop_loadProgram :: Bool
prop_loadProgram =
    let bs = [0xA9, 0x42] :: [Word8]  -- LDA #$42
        s = loadProgram 0x0600 bs initCPU
    in  view regPC s == 0x0600 &&
        view (memAt 0x0600) s == 0xA9 &&
        view (memAt 0x0601) s == 0x42
