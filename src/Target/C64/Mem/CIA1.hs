-- | CIA 1 registers ($DC00-$DC0F).
-- Keyboard, joystick port 2, datasette.
module Target.C64.Mem.CIA1 where

import Data.Word (Word16)

cia1DataPortA :: Word16
cia1DataPortA = 0xDC00

cia1DataPortB :: Word16
cia1DataPortB = 0xDC01

cia1DataDirA :: Word16
cia1DataDirA = 0xDC02

cia1DataDirB :: Word16
cia1DataDirB = 0xDC03

cia1TimerALo :: Word16
cia1TimerALo = 0xDC04

cia1TimerAHi :: Word16
cia1TimerAHi = 0xDC05

cia1TimerBLo :: Word16
cia1TimerBLo = 0xDC06

cia1TimerBHi :: Word16
cia1TimerBHi = 0xDC07

cia1TOD10ths :: Word16
cia1TOD10ths = 0xDC08

cia1TODSec :: Word16
cia1TODSec = 0xDC09

cia1TODMin :: Word16
cia1TODMin = 0xDC0A

cia1TODHour :: Word16
cia1TODHour = 0xDC0B

cia1SerialData :: Word16
cia1SerialData = 0xDC0C

cia1InterruptControl :: Word16
cia1InterruptControl = 0xDC0D

cia1ControlA :: Word16
cia1ControlA = 0xDC0E

cia1ControlB :: Word16
cia1ControlB = 0xDC0F
