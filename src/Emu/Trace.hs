module Emu.Trace (trace, runUntil, runN, loadProgram) where

import Data.Word (Word8, Word16)

import Emu.CPU (CPUState, set, over, regPC, mem)
import Emu.Mem (loadBytes)
import Emu.Step (step)

-- | Lazy infinite list of CPU states: current state followed by stepped states.
trace :: CPUState -> [CPUState]
trace s = s : trace (step s)

-- | Run until a predicate is satisfied, returning the first matching state.
runUntil :: (CPUState -> Bool) -> CPUState -> CPUState
runUntil p s
    | p s       = s
    | otherwise = runUntil p (step s)

-- | Run exactly N steps.
runN :: Int -> CPUState -> CPUState
runN 0 s = s
runN n s = runN (n - 1) (step s)

-- | Load program bytes at given address and set PC to that address.
loadProgram :: Word16 -> [Word8] -> CPUState -> CPUState
loadProgram addr bs = set regPC addr . over mem (loadBytes addr bs)
