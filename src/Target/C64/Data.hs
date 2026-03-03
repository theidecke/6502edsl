module Target.C64.Data
    ( -- * Generic data embedding (re-exported from Asm.Mos6502.Data)
      byte, word
      -- * C64-specific data embedding
    , petscii, pstring, charToPetscii
    ) where

import Data.Char (ord)
import Data.Word (Word8)

import Asm.Monad (MonadASM(..))
import Asm.Mos6502.Data (byte, word)

-- | Convert a Haskell 'String' to PETSCII (unshifted/uppercase mode)
-- and emit it.
petscii :: MonadASM m => String -> m ()
petscii s = emitBytes (map charToPetscii s)

-- | Emit a null-terminated PETSCII string.
pstring :: MonadASM m => String -> m ()
pstring s = petscii s >> emitBytes [0x00]

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
