-- | ROM routine labels for profiling and debugging.
--
-- Provides curated entry-point addresses for KERNAL jump table routines,
-- hardware vectors, and BASIC ROM floating-point routines.
module Target.C64.RomLabels
    ( -- * Combined label map
      romLabels
      -- * Partitioned label maps
    , kernalLabels
    , basicFpLabels
      -- * BASIC ROM floating-point routine addresses
    , romMOVMF, romMOVFM, romFACINX, romFADD, romFSUB, romFDIV, romFMULT
    , romGIVAYF, romMOVEF, romMOVFA, romFSUBT, romQINT, romCONUPK
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word16)

import Asm.Monad (AnnotationStack)

-- | All ROM labels: KERNAL jump table + BASIC FP routines.
romLabels :: Map Word16 AnnotationStack
romLabels = kernalLabels `Map.union` basicFpLabels

-- | KERNAL jump table entries ($FF81-$FFF3) and hardware vectors ($FFFA-$FFFE).
kernalLabels :: Map Word16 AnnotationStack
kernalLabels = Map.fromList
    [ (0xFF81, ["kernalInitEditor"])
    , (0xFF84, ["kernalInitIO"])
    , (0xFF87, ["kernalInitMemory"])
    , (0xFF8A, ["kernalRestoreVectors"])
    , (0xFF8D, ["kernalSetVectors"])
    , (0xFF90, ["kernalControlKernalMsg"])
    , (0xFF93, ["kernalSecondAfterListen"])
    , (0xFF96, ["kernalTalkSecondary"])
    , (0xFF99, ["kernalMemoryTop"])
    , (0xFF9C, ["kernalMemoryBottom"])
    , (0xFF9F, ["kernalScanKeyboard"])
    , (0xFFA2, ["kernalSetTimeout"])
    , (0xFFA5, ["kernalSerialByteIn"])
    , (0xFFA8, ["kernalSerialByteOut"])
    , (0xFFAB, ["kernalUntalk"])
    , (0xFFAE, ["kernalUnlisten"])
    , (0xFFB1, ["kernalListen"])
    , (0xFFB4, ["kernalTalk"])
    , (0xFFB7, ["kernalReadIOStatus"])
    , (0xFFBA, ["kernalSetLogicalFile"])
    , (0xFFBD, ["kernalSetFirstAddress"])
    , (0xFFC0, ["kernalSetSecondAddress"])
    , (0xFFC3, ["kernalInputFromDevice"])
    , (0xFFC6, ["kernalOutputToDevice"])
    , (0xFFC9, ["kernalRestoreIO"])
    , (0xFFCC, ["kernalInputChar"])
    , (0xFFCF, ["kernalOutputChar"])
    , (0xFFD2, ["kernalLoadFromDevice"])
    , (0xFFD5, ["kernalSaveToDevice"])
    , (0xFFD8, ["kernalSetClock"])
    , (0xFFDB, ["kernalReadClock"])
    , (0xFFDE, ["kernalCheckStopKey"])
    , (0xFFE1, ["kernalGetCharFromBuffer"])
    , (0xFFE4, ["kernalCloseAllFiles"])
    , (0xFFE7, ["kernalIncrementClock"])
    , (0xFFED, ["kernalScreenOrg"])
    , (0xFFF0, ["kernalReadKeyboard"])
    , (0xFFF3, ["kernalPlotCursor"])
    , (0xFFFA, ["vecNMI"])
    , (0xFFFC, ["vecReset"])
    , (0xFFFE, ["vecIRQ"])
    ]

-- | BASIC ROM floating-point routine entry points.
basicFpLabels :: Map Word16 AnnotationStack
basicFpLabels = Map.fromList
    [ (romMOVMF,  ["romMOVMF"])
    , (romMOVFM,  ["romMOVFM"])
    , (romFACINX, ["romFACINX"])
    , (romFADD,   ["romFADD"])
    , (romFSUB,   ["romFSUB"])
    , (romFDIV,   ["romFDIV"])
    , (romFMULT,  ["romFMULT"])
    , (romGIVAYF, ["romGIVAYF"])
    , (romMOVEF,  ["romMOVEF"])
    , (romMOVFA,  ["romMOVFA"])
    , (romFSUBT,  ["romFSUBT"])
    , (romQINT,   ["romQINT"])
    , (romCONUPK, ["romCONUPK"])
    ]

-- | Store FAC to RAM (X=Addr.LB, Y=Addr.HB)
romMOVMF :: Word16
romMOVMF = 0xBBD4

-- | Load FAC from RAM (A=Addr.LB, Y=Addr.HB)
romMOVFM :: Word16
romMOVFM = 0xBBA2

-- | FAC to 16-bit signed int (Y=LB, A=HB)
romFACINX :: Word16
romFACINX = 0xB1AA

-- | Add FAC + number in RAM (A=Addr.LB, Y=Addr.HB)
romFADD :: Word16
romFADD = 0xB867

-- | Subtract: FAC = Mem - FAC (A=Addr.LB, Y=Addr.HB)
romFSUB :: Word16
romFSUB = 0xB850

-- | Divide number in RAM by FAC (A=Addr.LB, Y=Addr.HB)
romFDIV :: Word16
romFDIV = 0xBB0F

-- | Multiply number from RAM * FAC (A=Addr.LB, Y=Addr.HB)
romFMULT :: Word16
romFMULT = 0xBA28

-- | Convert 16-bit signed to float in FAC (Y=LB, A=HB)
romGIVAYF :: Word16
romGIVAYF = 0xB391

-- | Copy ARG to FAC
romMOVEF :: Word16
romMOVEF = 0xBBFC

-- | Copy FAC to ARG
romMOVFA :: Word16
romMOVFA = 0xBC0F

-- | Subtract ARG from FAC: FAC = ARG - FAC
romFSUBT :: Word16
romFSUBT = 0xB853

-- | Convert FAC1 to 32 bit integer
romQINT :: Word16
romQINT = 0xBC9B

-- | Fill ARG with number from memory (A=Adr.LB, Y=Adr.HB).
romCONUPK :: Word16
romCONUPK = 0xBA8C
