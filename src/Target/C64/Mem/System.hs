-- | C64 system addresses: CPU port, system vectors, memory regions, colors.
module Target.C64.Mem.System where

import Data.Word (Word8, Word16)

-- CPU port ($00-$01, zero page)
cpuDataDirection :: Word8
cpuDataDirection = 0x00

cpuIOPort :: Word8
cpuIOPort = 0x01

-- System vectors ($0314-$0333)
sysIRQVec :: Word16
sysIRQVec = 0x0314

sysBRKVec :: Word16
sysBRKVec = 0x0316

sysNMIVec :: Word16
sysNMIVec = 0x0318

sysOpenVec :: Word16
sysOpenVec = 0x031A

sysCloseVec :: Word16
sysCloseVec = 0x031C

sysChkinVec :: Word16
sysChkinVec = 0x031E

sysChkoutVec :: Word16
sysChkoutVec = 0x0320

sysClrchVec :: Word16
sysClrchVec = 0x0322

sysBasinVec :: Word16
sysBasinVec = 0x0324

sysBsoutVec :: Word16
sysBsoutVec = 0x0326

sysStopVec :: Word16
sysStopVec = 0x0328

sysGetinVec :: Word16
sysGetinVec = 0x032A

sysClallVec :: Word16
sysClallVec = 0x032C

sysExmonVec :: Word16
sysExmonVec = 0x032E

sysLoadVec :: Word16
sysLoadVec = 0x0330

sysSaveVec :: Word16
sysSaveVec = 0x0332

-- Screen and color RAM
screenRAM :: Word16
screenRAM = 0x0400

colorRAM :: Word16
colorRAM = 0xD800

-- Region bases
basicROM :: Word16
basicROM = 0xA000

vicIOBase :: Word16
vicIOBase = 0xD000

sidIOBase :: Word16
sidIOBase = 0xD400

colorRAMBase :: Word16
colorRAMBase = 0xD800

cia1IOBase :: Word16
cia1IOBase = 0xDC00

cia2IOBase :: Word16
cia2IOBase = 0xDD00

kernalROM :: Word16
kernalROM = 0xE000

-- Color constants (Word8, for immediate mode)
colorBlack :: Word8
colorBlack = 0x00

colorWhite :: Word8
colorWhite = 0x01

colorRed :: Word8
colorRed = 0x02

colorCyan :: Word8
colorCyan = 0x03

colorPurple :: Word8
colorPurple = 0x04

colorGreen :: Word8
colorGreen = 0x05

colorBlue :: Word8
colorBlue = 0x06

colorYellow :: Word8
colorYellow = 0x07

colorOrange :: Word8
colorOrange = 0x08

colorBrown :: Word8
colorBrown = 0x09

colorLightRed :: Word8
colorLightRed = 0x0A

colorDarkGrey :: Word8
colorDarkGrey = 0x0B

colorMediumGrey :: Word8
colorMediumGrey = 0x0C

colorLightGreen :: Word8
colorLightGreen = 0x0D

colorLightBlue :: Word8
colorLightBlue = 0x0E

colorLightGrey :: Word8
colorLightGrey = 0x0F
