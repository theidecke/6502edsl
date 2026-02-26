module Target.C64.PRG
    ( toPRG
    ) where

import Data.Word (Word8, Word16)

import Asm.Monad (lo, hi)

-- | Wrap raw machine code bytes in a C64 PRG container.
-- Prepends the 2-byte little-endian load address header.
toPRG :: Word16 -> [Word8] -> [Word8]
toPRG addr bytes = lo addr : hi addr : bytes
