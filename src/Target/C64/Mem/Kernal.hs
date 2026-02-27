-- | Kernal jump table ($FF81-$FFF3) and hardware vectors ($FFFA-$FFFE).
module Target.C64.Mem.Kernal where

import Data.Word (Word16)

-- Kernal jump table entries ($FF81-$FFF3)

kernalInitEditor :: Word16
kernalInitEditor = 0xFF81

kernalInitIO :: Word16
kernalInitIO = 0xFF84

kernalInitMemory :: Word16
kernalInitMemory = 0xFF87

kernalRestoreVectors :: Word16
kernalRestoreVectors = 0xFF8A

kernalSetVectors :: Word16
kernalSetVectors = 0xFF8D

kernalControlKernalMsg :: Word16
kernalControlKernalMsg = 0xFF90

kernalSecondAfterListen :: Word16
kernalSecondAfterListen = 0xFF93

kernalTalkSecondary :: Word16
kernalTalkSecondary = 0xFF96

kernalMemoryTop :: Word16
kernalMemoryTop = 0xFF99

kernalMemoryBottom :: Word16
kernalMemoryBottom = 0xFF9C

kernalScanKeyboard :: Word16
kernalScanKeyboard = 0xFF9F

kernalSetTimeout :: Word16
kernalSetTimeout = 0xFFA2

kernalSerialByteIn :: Word16
kernalSerialByteIn = 0xFFA5

kernalSerialByteOut :: Word16
kernalSerialByteOut = 0xFFA8

kernalUntalk :: Word16
kernalUntalk = 0xFFAB

kernalUnlisten :: Word16
kernalUnlisten = 0xFFAE

kernalListen :: Word16
kernalListen = 0xFFB1

kernalTalk :: Word16
kernalTalk = 0xFFB4

kernalReadIOStatus :: Word16
kernalReadIOStatus = 0xFFB7

kernalSetLogicalFile :: Word16
kernalSetLogicalFile = 0xFFBA

kernalSetFirstAddress :: Word16
kernalSetFirstAddress = 0xFFBD

kernalSetSecondAddress :: Word16
kernalSetSecondAddress = 0xFFC0

kernalInputFromDevice :: Word16
kernalInputFromDevice = 0xFFC3

kernalOutputToDevice :: Word16
kernalOutputToDevice = 0xFFC6

kernalRestoreIO :: Word16
kernalRestoreIO = 0xFFC9

kernalInputChar :: Word16
kernalInputChar = 0xFFCC

kernalOutputChar :: Word16
kernalOutputChar = 0xFFCF

kernalLoadFromDevice :: Word16
kernalLoadFromDevice = 0xFFD2

kernalSaveToDevice :: Word16
kernalSaveToDevice = 0xFFD5

kernalSetClock :: Word16
kernalSetClock = 0xFFD8

kernalReadClock :: Word16
kernalReadClock = 0xFFDB

kernalCheckStopKey :: Word16
kernalCheckStopKey = 0xFFDE

kernalGetCharFromBuffer :: Word16
kernalGetCharFromBuffer = 0xFFE1

kernalCloseAllFiles :: Word16
kernalCloseAllFiles = 0xFFE4

kernalIncrementClock :: Word16
kernalIncrementClock = 0xFFE7

kernalScreenOrg :: Word16
kernalScreenOrg = 0xFFED

kernalReadKeyboard :: Word16
kernalReadKeyboard = 0xFFF0

kernalPlotCursor :: Word16
kernalPlotCursor = 0xFFF3

-- Hardware vectors

vecNMI :: Word16
vecNMI = 0xFFFA

vecReset :: Word16
vecReset = 0xFFFC

vecIRQ :: Word16
vecIRQ = 0xFFFE
