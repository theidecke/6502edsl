module Asm.Mos6502.Memory (align, alignPage) where

import Asm.Monad (ASM, emit, label)

-- | Pad with @0x00@ bytes until the program counter is a multiple of @n@.
align :: Int -> ASM ()
align n
    | n <= 0    = pure ()
    | otherwise = do
        pc <- label
        let pos       = fromIntegral pc :: Int
            remainder = pos `mod` n
            padding   = if remainder == 0 then 0 else n - remainder
        emit (replicate padding 0x00)

-- | @align 256@. Ensures page-aligned data (useful for lookup tables
-- that must not cross page boundaries).
alignPage :: ASM ()
alignPage = align 256
