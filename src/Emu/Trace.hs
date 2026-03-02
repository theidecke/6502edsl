module Emu.Trace
    ( trace, runUntil, runN, loadProgram, traceForCycles
    , watchReg, watchMem, watch16, deltas, pcCoverage
    , findLastGoodStates, formatStateTable, formatState
    , formatProfile, displayStack
    , hex8, hex16
    , Map
    ) where

import Control.Exception (evaluate, try, SomeException)
import Data.Bits (shiftL, (.|.))
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..))
import Data.Word (Word8, Word16)
import Numeric (showHex, showFFloat)

import Asm.Monad (AnnotationStack)
import Emu.CPU (CPUState, Lens', view, set, over,
                regPC, regA, regX, regY, regSP, cycles, mem, memAt)
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

-- | Trace states up to a cycle limit (exclusive).
traceForCycles :: Int -> CPUState -> [CPUState]
traceForCycles maxCycles = takeWhile (\s -> view cycles s < maxCycles) . trace

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

-- | PC coverage histogram: maps each visited address to its execution count.
pcCoverage :: [CPUState] -> Map Word16 Int
pcCoverage = foldl' (\m s -> Map.insertWith (+) (view regPC s) 1 m) Map.empty

-- ---------------------------------------------------------------------------
-- Annotation stack display
-- ---------------------------------------------------------------------------

-- | Render an annotation stack as a path string (outermost first).
-- @["inner","outer"]@ becomes @"outer/inner"@.
displayStack :: AnnotationStack -> String
displayStack [] = ""
displayStack xs = intercalate "/" (reverse xs)

-- ---------------------------------------------------------------------------
-- Debugging helpers
-- ---------------------------------------------------------------------------

-- | Scan a trace, keeping the last @n@ states.  Stops when forcing the next
-- state throws an exception (e.g. illegal opcode).
-- Returns @(totalSteps, lastWindow)@.
findLastGoodStates :: Int -> [CPUState] -> IO (Int, [CPUState])
findLastGoodStates windowSize = go 0 []
  where
    go !n window [] = pure (n, window)
    go !n window (s:ss) = do
        result <- try @SomeException (evaluate (view cycles s))
        case result of
            Left _  -> pure (n, window)
            Right _ -> go (n + 1) (takeLast windowSize (window ++ [s])) ss
    takeLast k xs = drop (max 0 (length xs - k)) xs

-- | Format a single CPU state as a debug line.
-- Takes a label map (address -> annotation stack), step index, and state.
formatState :: Map Word16 AnnotationStack -> Int -> CPUState -> String
formatState labelMap i s =
    "  " ++ padR 5 (show i)
        ++ "  $" ++ hex16 (view regPC s)
        ++ "  " ++ hex8  (view regA s)
        ++ "  " ++ hex8  (view regX s)
        ++ "  " ++ hex8  (view regY s)
        ++ "  $" ++ hex8  (view regSP s)
        ++ "  " ++ padR 6 (show (view cycles s))
        ++ "  [" ++ hex8  (view (memAt (view regPC s)) s) ++ "]"
        ++ lookupLabel (view regPC s)
  where
    lookupLabel addr = case Map.lookupLE addr labelMap of
        Just (_, xs) | not (null xs) -> "  (" ++ displayStack xs ++ ")"
        _                            -> ""

-- | Format a window of states as a debug table (header + rows).
-- @startIdx@ is the step number of the first state in the window.
formatStateTable :: Map Word16 AnnotationStack -> Int -> [CPUState] -> String
formatStateTable labelMap startIdx states = unlines $
    [ "  Step   PC      A   X   Y   SP    Cycles  Label"
    , "  -----  ------  --  --  --  ----  ------  -----"
    ] ++ zipWith (formatState labelMap) [startIdx..] states

-- | Format a PC coverage map as an execution profile, sorted by hit count
-- descending.  Each line shows the address, optional label, hit count, and
-- percentage of total steps.
formatProfile :: Map Word16 AnnotationStack -> Map Word16 Int -> String
formatProfile labelMap cov = unlines $
    [ "Execution profile (" ++ show totalSteps ++ " steps, "
        ++ show (Map.size cov) ++ " unique addresses)"
    , ""
    , "  Address  Label                          Hits  %"
    , "  -------  ----------------------------  -----  ------"
    ] ++ map fmtRow sorted
  where
    totalSteps = sum (Map.elems cov)
    sorted = sortBy (\a b -> compare (Down (snd a)) (Down (snd b))) (Map.toList cov)
    fmtRow (addr, hits) =
        let lbl   = maybe "" (displayStack . snd) (Map.lookupLE addr labelMap)
            pct   = 100 * fromIntegral hits / fromIntegral totalSteps :: Double
            pctS  = showFFloat (Just 1) pct "%"
        in  "  $" ++ hex16 addr
                ++ "  " ++ padR 28 lbl
                ++ "  " ++ padR 5 (show hits)
                ++ "  " ++ pctS

-- | Format a 'Word8' as a zero-padded 2-digit hex string.
hex8 :: Word8 -> String
hex8 w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

-- | Format a 'Word16' as a zero-padded 4-digit hex string.
hex16 :: Word16 -> String
hex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
