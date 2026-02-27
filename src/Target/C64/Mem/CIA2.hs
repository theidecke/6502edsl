-- | CIA 2 registers ($DD00-$DD0F).
-- VIC bank select, serial bus, RS-232.
module Target.C64.Mem.CIA2 where

import Data.Word (Word16)

cia2DataPortA :: Word16
cia2DataPortA = 0xDD00

cia2DataPortB :: Word16
cia2DataPortB = 0xDD01

cia2DataDirA :: Word16
cia2DataDirA = 0xDD02

cia2DataDirB :: Word16
cia2DataDirB = 0xDD03

cia2TimerALo :: Word16
cia2TimerALo = 0xDD04

cia2TimerAHi :: Word16
cia2TimerAHi = 0xDD05

cia2TimerBLo :: Word16
cia2TimerBLo = 0xDD06

cia2TimerBHi :: Word16
cia2TimerBHi = 0xDD07

cia2TOD10ths :: Word16
cia2TOD10ths = 0xDD08

cia2TODSec :: Word16
cia2TODSec = 0xDD09

cia2TODMin :: Word16
cia2TODMin = 0xDD0A

cia2TODHour :: Word16
cia2TODHour = 0xDD0B

cia2SerialData :: Word16
cia2SerialData = 0xDD0C

cia2InterruptControl :: Word16
cia2InterruptControl = 0xDD0D

cia2ControlA :: Word16
cia2ControlA = 0xDD0E

cia2ControlB :: Word16
cia2ControlB = 0xDD0F
