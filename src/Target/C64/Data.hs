module Target.C64.Data (byte, word, petscii, pstring, charToPetscii) where

import Data.Char (ord)
import Data.Word (Word8, Word16)

import Asm.Monad (ASM, emit, lo, hi)

-- | Emit raw bytes into the output stream.
byte :: [Word8] -> ASM ()
byte = emit

-- | Emit 16-bit words as little-endian byte pairs.
word :: [Word16] -> ASM ()
word ws = emit (concatMap (\w -> [lo w, hi w]) ws)

-- | Convert a Haskell 'String' to PETSCII (unshifted/uppercase mode)
-- and emit it.
petscii :: String -> ASM ()
petscii s = emit (map charToPetscii s)

-- | Emit a null-terminated PETSCII string.
pstring :: String -> ASM ()
pstring s = petscii s >> emit [0x00]

-- | Convert an ASCII 'Char' to a PETSCII byte (unshifted/uppercase mode).
--
-- Mapping:
--
-- * @a@\u2013@z@ \u2192 @0xC1@\u2013@0xDA@ (PETSCII lowercase)
-- * Other printable ASCII (@0x20@\u2013@0x7E@) \u2192 passed through as-is
-- * Non-printable or out-of-range \u2192 @error@
charToPetscii :: Char -> Word8
charToPetscii c
    | code >= 0x61 && code <= 0x7A = fromIntegral (code - 0x61 + 0xC1)
    | code >= 0x20 && code <= 0x7E = fromIntegral code
    | otherwise = error $ "charToPetscii: unsupported character "
                       ++ show c ++ " (code " ++ show code ++ ")"
  where
    code = ord c
