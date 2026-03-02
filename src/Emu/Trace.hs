module Emu.Trace
    ( trace, runUntil, runN, loadProgram
    , watchReg, watchMem, watch16, deltas
    ) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8, Word16)

import Emu.CPU (CPUState, Lens', view, set, over, regPC, mem, memAt)
import Emu.Mem (loadBytes, diffMem)
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

-- ---------------------------------------------------------------------------
-- Observation combinators
-- ---------------------------------------------------------------------------

-- | Watch a register (or any lens) evolve over the trace.
watchReg :: Lens' CPUState a -> [CPUState] -> [a]
watchReg l = map (view l)

-- | Watch a memory address evolve over the trace.
watchMem :: Word16 -> [CPUState] -> [Word8]
watchMem addr = map (view (memAt addr))

-- | Read a 16-bit little-endian value from two consecutive addresses.
watch16 :: Word8 -> CPUState -> Word16
watch16 zpAddr s =
    let lo = view (memAt (fromIntegral zpAddr)) s
        hi = view (memAt (fromIntegral zpAddr + 1)) s
    in  fromIntegral hi `shiftL` 8 .|. fromIntegral lo

-- | Memory diffs between consecutive trace states.
deltas :: [CPUState] -> [[(Word16, Word8, Word8)]]
deltas states = zipWith (\a b -> diffMem (view mem a) (view mem b)) states (drop 1 states)
