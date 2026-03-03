module Backend.C64.ViceLabels (exportViceLabels) where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import Numeric (showHex)

import Asm.Monad (AnnotationStack)

-- | Generate VICE monitor symbol file content from a label map.
-- Empty annotation stacks are silently dropped.
exportViceLabels :: Map Word16 AnnotationStack -> String
exportViceLabels = concatMap formatLabel . Map.toAscList . Map.filter (not . null)
  where
    formatLabel (addr, stack) =
        "al C:" ++ showHex16 addr ++ " ." ++ intercalate "/" (reverse stack) ++ "\n"

    showHex16 :: Word16 -> String
    showHex16 w = pad (showHex w "")
      where
        pad s = replicate (4 - length s) '0' ++ s
