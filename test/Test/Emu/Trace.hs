module Test.Emu.Trace (tests) where

import Data.Word (Word8)
import Test.QuickCheck

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
    , section "Emu.Trace (combinators)"
    , check "watchReg regA on NOPs"          prop_watchRegA
    , check "watchReg regPC on NOPs"         prop_watchRegPC
    , check "watchMem tracks STA"            prop_watchMem
    , check "watch16 little-endian"          prop_watch16
    , check "deltas NOPs are empty"          prop_deltasNop
    , check "deltas STA produces diff"       prop_deltasSta
    , check "deltas length"                  prop_deltasLength
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

-- ---------------------------------------------------------------------------
-- Combinator properties
-- ---------------------------------------------------------------------------

-- | watchReg regA on 3 NOPs: A stays 0 across all 4 states (initial + 3 steps)
prop_watchRegA :: Bool
prop_watchRegA =
    let s0 = loadProgram 0x0800 [0xEA, 0xEA, 0xEA] initCPU
        t  = take 4 (trace s0)
    in  watchReg regA t == [0, 0, 0, 0]

-- | watchReg regPC on 3 NOPs: PC advances 0800, 0801, 0802, 0803
prop_watchRegPC :: Bool
prop_watchRegPC =
    let s0 = loadProgram 0x0800 [0xEA, 0xEA, 0xEA] initCPU
        t  = take 4 (trace s0)
    in  watchReg regPC t == [0x0800, 0x0801, 0x0802, 0x0803]

-- | watchMem tracks a memory location through LDA #val; STA addr
prop_watchMem :: Bool
prop_watchMem =
    -- LDA #$42; STA $0300; NOP
    let s0 = loadProgram 0x0800 [0xA9, 0x42, 0x8D, 0x00, 0x03, 0xEA] initCPU
        t  = take 4 (trace s0)  -- initial, after LDA, after STA, after NOP
        vals = watchMem 0x0300 t
    in  vals == [0, 0, 0x42, 0x42]

-- | watch16 reads two ZP bytes as little-endian Word16
prop_watch16 :: Word8 -> Property
prop_watch16 zpAddr =
    zpAddr < 0xFF ==>
    let s = set (memAt (fromIntegral zpAddr)) 0x34
          . set (memAt (fromIntegral zpAddr + 1)) 0x12
          $ initCPU
    in  watch16 zpAddr s == 0x1234

-- | NOPs produce empty deltas (no memory changes visible at the Mem level)
prop_deltasNop :: Bool
prop_deltasNop =
    let s0 = loadProgram 0x0800 [0xEA, 0xEA, 0xEA] initCPU
        t  = take 4 (trace s0)
        ds = deltas t
    in  all null ds

-- | STA produces a delta with the written address
prop_deltasSta :: Bool
prop_deltasSta =
    -- LDA #$42; STA $0300
    let s0 = loadProgram 0x0800 [0xA9, 0x42, 0x8D, 0x00, 0x03] initCPU
        t  = take 3 (trace s0)  -- initial, after LDA, after STA
        ds = deltas t            -- [delta01, delta12]
    in  null (ds !! 0) &&        -- LDA doesn't write memory
        (0x0300, 0, 0x42) `elem` (ds !! 1)  -- STA writes $42

-- | deltas length matches number of transitions
prop_deltasLength :: Positive Int -> Bool
prop_deltasLength (Positive n') =
    let n = min n' 50  -- keep it reasonable
        -- Fill with NOPs so we can step safely
        s0 = loadProgram 0x0800 (replicate (n + 1) 0xEA) initCPU
        t  = take (n + 1) (trace s0)
    in  length (deltas t) == n
