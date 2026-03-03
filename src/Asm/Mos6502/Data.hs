module Asm.Mos6502.Data (byte, word) where

import Data.Word (Word8, Word16)

import Asm.Monad (MonadASM(..), lo, hi)

-- | Emit raw bytes into the output stream.
byte :: MonadASM m => [Word8] -> m ()
byte = emitBytes

-- | Emit 16-bit words as little-endian byte pairs.
word :: MonadASM m => [Word16] -> m ()
word ws = emitBytes (concatMap (\w -> [lo w, hi w]) ws)
