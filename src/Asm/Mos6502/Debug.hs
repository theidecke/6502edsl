module Asm.Mos6502.Debug (fitsIn, annotate) where

import Control.Monad (when)

import Asm.Monad (ASM, label, recordLabel)

-- | Assert that a block emits at most @maxBytes@ bytes.
-- Returns the block's result so it composes transparently.
fitsIn :: Int -> ASM a -> ASM a
fitsIn maxBytes block = do
    startPC <- label
    a <- block
    endPC <- label
    let size = fromIntegral (endPC - startPC) :: Int
    when (size > maxBytes) $
        error $ "fitsIn: block emitted " ++ show size
             ++ " bytes, limit was " ++ show maxBytes
    pure a

-- | Record the start address of a block as a named label, then run the block.
-- The label can later be extracted via 'assembleWithLabels'.
annotate :: String -> ASM a -> ASM a
annotate name block = do
    pc <- label
    recordLabel name pc
    block
