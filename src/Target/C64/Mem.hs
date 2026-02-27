-- | C64 memory map: re-exports all submodules.
module Target.C64.Mem
    ( module Target.C64.Mem.VIC
    , module Target.C64.Mem.SID
    , module Target.C64.Mem.CIA1
    , module Target.C64.Mem.CIA2
    , module Target.C64.Mem.Kernal
    , module Target.C64.Mem.System
    ) where

import Target.C64.Mem.VIC
import Target.C64.Mem.SID
import Target.C64.Mem.CIA1
import Target.C64.Mem.CIA2
import Target.C64.Mem.Kernal
import Target.C64.Mem.System
