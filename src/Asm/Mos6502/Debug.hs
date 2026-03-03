module Asm.Mos6502.Debug (fitsIn, annotate) where

import Control.Monad (when)

import Asm.Monad (ASM, currentPC, pushAnnotation, popAnnotation)

-- | Assert that a block emits at most @maxBytes@ bytes.
-- Returns the block's result so it composes transparently.
fitsIn :: Int -> ASM a -> ASM a
fitsIn maxBytes block = do
    startPC <- currentPC
    a <- block
    endPC <- currentPC
    let size = fromIntegral (endPC - startPC) :: Int
    when (size > maxBytes) $
        error $ "fitsIn: block emitted " ++ show size
             ++ " bytes, limit was " ++ show maxBytes
    pure a

-- | Run a block under a named annotation.  Pushes the name onto the
-- annotation stack so that every 'emit' inside records the nesting.
annotate :: String -> ASM a -> ASM a
annotate name block = do
    pushAnnotation name
    result <- block
    popAnnotation
    pure result
