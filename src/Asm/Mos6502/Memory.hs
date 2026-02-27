module Asm.Mos6502.Memory (align, alignPage, samePage) where

import Data.Bits ((.&.))
import Data.Word (Word16)
import Control.Monad (when)
import Numeric (showHex)

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

-- | Assert that the current PC and the given address are on the same
-- 256-byte page. Fails at assembly time if they differ.
samePage :: Word16 -> ASM ()
samePage addr = do
    pc <- label
    when ((pc .&. 0xFF00) /= (addr .&. 0xFF00)) $
        error $ "samePage: PC=$" ++ showHex16 pc
             ++ " and address=$" ++ showHex16 addr
             ++ " are on different pages"
  where
    showHex16 :: Word16 -> String
    showHex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s
