{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.List (find)
import Data.Word (Word8, Word16)
import Numeric (showHex)

import Asm.Monad (ASM, TargetConfig(..), assemble, label)
import Asm.Mos6502
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), encode)
import Emu.CPU
import Emu.Mem (diffMem)
import Emu.Trace

import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Assemble a program at a given origin.
asm :: Word16 -> ASM a -> [Word8]
asm org = snd . assemble TargetConfig { origin = org, freeZeroPage = Set.empty }

-- | Assemble with a ZP free list for variable allocation.
asmZP :: Word16 -> ASM a -> [Word8]
asmZP org = snd . assemble TargetConfig { origin = org, freeZeroPage = Set.fromList [0x02..0x0F] }

-- | Load assembled bytes at origin, return initial state.
load :: Word16 -> [Word8] -> CPUState
load org bs = loadProgram org bs initCPU

hex8 :: Word8 -> String
hex8 w = "$" ++ pad 2 (showHex w "")

hex16 :: Word16 -> String
hex16 w = "$" ++ pad 4 (showHex w "")

pad :: Int -> String -> String
pad n s = replicate (n - length s) '0' ++ s

heading :: String -> IO ()
heading s = do
    putStrLn ""
    putStrLn $ replicate 60 '-'
    putStrLn $ "  " ++ s
    putStrLn $ replicate 60 '-'

-- ---------------------------------------------------------------------------
-- 1. Register Evolution
-- ---------------------------------------------------------------------------

demo1_registerEvolution :: IO ()
demo1_registerEvolution = do
    heading "1. Register Evolution: Countdown Loop"
    putStrLn "  Program: LDX #$05; loop: DEX; BNE loop"
    putStrLn ""

    -- Assemble: LDX #$05; DEX; BNE loop
    -- Layout: LDX#(2 bytes) at $0800, DEX(1) at $0802, BNE(2) at $0803
    -- Loop runs 5 iterations: 1 LDX + 5*(DEX+BNE) = 11 steps, 12 states
    let bs = asm 0x0800 $ mdo
                ldx # (0x05 :: Word8)
                loop <- label
                dex
                bne loop
        s0 = load 0x0800 bs
        t  = take 12 (trace s0)
        xs = watchReg regX t

    putStrLn $ "  X register over 12 states: " ++ show xs
    putStrLn $ "  (initial=0, then LDX sets 5, then DEX/BNE pairs count down)"

-- ---------------------------------------------------------------------------
-- 2. Memory Observation
-- ---------------------------------------------------------------------------

demo2_memoryObservation :: IO ()
demo2_memoryObservation = do
    heading "2. Memory Observation: Tracking Writes"
    putStrLn "  Program: LDA #$42; STA $0300; LDA #$99; STA $0300"
    putStrLn ""

    let bs = asm 0x0800 $ do
                lda # (0x42 :: Word8)
                sta (0x0300 :: Word16)
                lda # (0x99 :: Word8)
                sta (0x0300 :: Word16)
        s0 = load 0x0800 bs
        t  = take 5 (trace s0)  -- initial + 4 instructions
        vals = watchMem 0x0300 t

    putStrLn $ "  $0300 over time: " ++ show (map hex8 vals)
    putStrLn $ "  (starts at $00, becomes $42 after first STA, then $99 after second)"

-- ---------------------------------------------------------------------------
-- 3. 16-bit Variable Tracking
-- ---------------------------------------------------------------------------

demo3_watch16 :: IO ()
demo3_watch16 = do
    heading "3. 16-bit Variable Tracking"
    putStrLn "  Program: Manual 16-bit increment at ZP $02/$03"
    putStrLn ""

    -- INC $02; BNE skip; INC $03; skip: NOP  (repeated 3 times)
    -- Start with $02=FE, $03=00 to see carry propagation
    let incBytes = encode (Instruction INC (ZeroPage 0x02))
                ++ encode (Instruction BNE (Relative 2))
                ++ encode (Instruction INC (ZeroPage 0x03))
                ++ encode (Instruction NOP Implied)
        bs = concat (replicate 3 incBytes)
        s0 = set (memAt 0x02) 0xFE . set (memAt 0x03) 0x00
           $ load 0x0800 bs
        -- Each inc16 is either 2 or 3 instructions (depending on carry)
        -- Take enough trace states to cover all iterations
        t = take 12 (trace s0)
        vals = map (watch16 0x02) t

    putStrLn $ "  16-bit value at $02: " ++ show (map hex16 vals)
    putStrLn $ "  (starts $00FE, increments through $00FF, $0100, $0101)"

-- ---------------------------------------------------------------------------
-- 4. Time-Travel Debugging
-- ---------------------------------------------------------------------------

demo4_timeTravelDebug :: IO ()
demo4_timeTravelDebug = do
    heading "4. Time-Travel Debugging"
    putStrLn "  Finding when a 'buggy' write first touches $0400"
    putStrLn ""

    -- Program writes to several addresses; one of them is $0400
    let bs = asm 0x0800 $ do
                lda # (0x01 :: Word8)
                sta (0x0300 :: Word16)
                lda # (0x02 :: Word8)
                sta (0x0301 :: Word16)
                lda # (0xFF :: Word8)      -- the "bug": wrong value
                sta (0x0400 :: Word16)
                lda # (0x03 :: Word8)
                sta (0x0302 :: Word16)
        s0 = load 0x0800 bs
        t  = take 9 (trace s0)
        ds = deltas t
        -- Find which step first writes to $0400
        indexed = zip [0 :: Int ..] ds
        bugStep = find (\(_, d) -> any (\(a,_,_) -> a == 0x0400) d) indexed

    case bugStep of
        Just (i, d) -> do
            let before = t !! i
                after  = t !! (i + 1)
            putStrLn $ "  Bug found at step " ++ show i ++ ":"
            putStrLn $ "    Delta: " ++ show [(hex16 a, hex8 o, hex8 n) | (a,o,n) <- d]
            putStrLn $ "    PC before: " ++ hex16 (view regPC before)
            putStrLn $ "    PC after:  " ++ hex16 (view regPC after)
            putStrLn $ "    A register: " ++ hex8 (view regA before)
            putStrLn $ "  -> The instruction at " ++ hex16 (view regPC before) ++ " wrote $FF to $0400"
        Nothing ->
            putStrLn "  (no write to $0400 found)"

-- ---------------------------------------------------------------------------
-- 5. Cross-Program RAM Diff
-- ---------------------------------------------------------------------------

demo5_crossProgramDiff :: IO ()
demo5_crossProgramDiff = do
    heading "5. Cross-Program RAM Diff"
    putStrLn "  Comparing two programs that compute 5*3=15 differently"
    putStrLn ""

    -- Program A: repeated addition (5+5+5)
    let bsA = asm 0x0800 $ do
                lda # (0x00 :: Word8)
                clc
                adc # (0x05 :: Word8)
                clc
                adc # (0x05 :: Word8)
                clc
                adc # (0x05 :: Word8)
                sta (0x0300 :: Word16)   -- result
    -- Program B: different approach (3+3+3+3+3)
        bsB = asm 0x0800 $ do
                lda # (0x00 :: Word8)
                clc
                adc # (0x03 :: Word8)
                clc
                adc # (0x03 :: Word8)
                clc
                adc # (0x03 :: Word8)
                clc
                adc # (0x03 :: Word8)
                clc
                adc # (0x03 :: Word8)
                sta (0x0300 :: Word16)

    let sA = runN 8 (load 0x0800 bsA)    -- LDA + 3*(CLC+ADC) + STA = 8
        sB = runN 12 (load 0x0800 bsB)   -- LDA + 5*(CLC+ADC) + STA = 12
        diffs = diffMem (view mem sA) (view mem sB)

    -- Filter out program ROM diffs (the programs themselves differ)
    let dataDiffs = filter (\(a,_,_) -> a >= 0x0300 && a < 0x0800) diffs

    putStrLn $ "  Program A: 5+5+5 = " ++ hex8 (view (memAt 0x0300) sA)
    putStrLn $ "  Program B: 3+3+3+3+3 = " ++ hex8 (view (memAt 0x0300) sB)
    putStrLn $ "  Data region diffs (>=$0300, <$0800): " ++ show [(hex16 a, hex8 o, hex8 n) | (a,o,n) <- dataDiffs]
    putStrLn $ "  Total diffs (including ROM): " ++ show (length diffs)
    putStrLn "  -> Both programs compute the same result ($0F = 15), only ROM differs"

-- ---------------------------------------------------------------------------
-- 6. Trace Bisection
-- ---------------------------------------------------------------------------

-- | Binary search for the first trace index where a predicate becomes true.
bisectTrace :: (CPUState -> Bool) -> [CPUState] -> Int
bisectTrace p states = go 0 (length statesV - 1)
  where
    statesV = states  -- we index with (!!)
    go lo hi
        | lo >= hi  = lo
        | p (statesV !! mid) = go lo mid
        | otherwise          = go (mid + 1) hi
      where mid = (lo + hi) `div` 2

demo6_traceBisection :: IO ()
demo6_traceBisection = do
    heading "6. Trace Bisection"
    putStrLn "  Finding the exact step that first writes to screen memory ($0400)"
    putStrLn ""

    -- Program: do some arithmetic, then write to screen at $0400
    let bs = asm 0x0800 $ do
                ldx # (0x00 :: Word8)
                inx; inx; inx; inx; inx   -- 5 INX instructions
                txa
                sta (0x0400 :: Word16)     -- write to screen
                nop
        s0 = load 0x0800 bs
        t  = take 20 (trace s0)
        -- Predicate: screen address has been written
        hasScreenWrite s = view (memAt 0x0400) s /= 0
        step_idx = bisectTrace hasScreenWrite t

    putStrLn $ "  Total trace states examined: " ++ show (length t)
    putStrLn $ "  First screen write at step: " ++ show step_idx
    putStrLn $ "  State before (step " ++ show (step_idx - 1) ++ "):"
    putStrLn $ "    A = " ++ hex8 (view regA (t !! (step_idx - 1)))
    putStrLn $ "    $0400 = " ++ hex8 (view (memAt 0x0400) (t !! (step_idx - 1)))
    putStrLn $ "  State at (step " ++ show step_idx ++ "):"
    putStrLn $ "    A = " ++ hex8 (view regA (t !! step_idx))
    putStrLn $ "    $0400 = " ++ hex8 (view (memAt 0x0400) (t !! step_idx))
    putStrLn $ "  -> bisectTrace pinpointed the STA instruction in O(log n) probes"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== 6502 Trace Explorer ==="
    putStrLn "Demonstrating lazy trace observation and time-travel debugging"
    demo1_registerEvolution
    demo2_memoryObservation
    demo3_watch16
    demo4_timeTravelDebug
    demo5_crossProgramDiff
    demo6_traceBisection
    putStrLn ""
    putStrLn "Done."
