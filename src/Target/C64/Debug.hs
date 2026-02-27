module Target.C64.Debug (exportViceLabels) where

import Data.Word (Word16)
import Numeric (showHex)

-- | Generate VICE monitor symbol file content from a list of (name, address)
-- pairs. Each line has the format: @al C:HHHH .name@
exportViceLabels :: [(String, Word16)] -> String
exportViceLabels = concatMap formatLabel
  where
    formatLabel (name, addr) =
        "al C:" ++ showHex16 addr ++ " ." ++ name ++ "\n"

    showHex16 :: Word16 -> String
    showHex16 w = pad (showHex w "")
      where
        pad s = replicate (4 - length s) '0' ++ s
