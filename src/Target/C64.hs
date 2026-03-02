module Target.C64
    ( C64Subsystems (..)
    , defaultC64Subsystems
    , c64TargetConfig
    , loadC64Roms
    ) where

import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8, Word16)

import Asm.Monad (TargetConfig(..))
import Emu.CPU (CPUState, over, mem)
import Emu.Mem (loadBytes)
import Target.C64.Mem.System (basicROM, chargenROM, kernalROM)

-- | Which C64 subsystems are active and therefore occupy zero page addresses.
data C64Subsystems = C64Subsystems
    { useBasic     :: Bool  -- ^ BASIC ROM interpreter
    , useKernal    :: Bool  -- ^ KERNAL ROM (core I/O: file tables, load/save, device info)
    , useCassette  :: Bool  -- ^ Datasette / tape
    , useRS232     :: Bool  -- ^ RS-232 serial
    , useKernalIRQ :: Bool  -- ^ Default KERNAL IRQ (jiffy clock, keyboard, cursor, screen editor)
    }
    deriving (Show)

-- | Conservative default: all subsystems enabled, minimal free zero page.
defaultC64Subsystems :: C64Subsystems
defaultC64Subsystems = C64Subsystems
    { useBasic     = True
    , useKernal    = True
    , useCassette  = True
    , useRS232     = True
    , useKernalIRQ = True
    }

-- | Build a 'TargetConfig' from a C64 subsystem selection and an origin address.
--
-- Determines which zero page addresses are free based on which subsystems
-- are disabled. When the KERNAL is off entirely, all its sub-components
-- (cassette, RS-232, IRQ) are implicitly off as well.
c64TargetConfig :: Word16 -> C64Subsystems -> TargetConfig
c64TargetConfig org subs = TargetConfig
    { origin = org
    , freeZeroPage = alwaysFree
        <> basicFree
        <> kernalFree
    , kernalRom  = Nothing
    , basicRom   = Nothing
    , chargenRom = Nothing
    }
  where
    alwaysFree :: Set Word8
    alwaysFree = Set.fromList $ [0x02] ++ [0xFB .. 0xFE]

    basicFree :: Set Word8
    basicFree
        | useBasic subs = Set.empty
        | otherwise     = Set.fromList $ [0x03 .. 0x8F] ++ [0xFF]

    kernalFree :: Set Word8
    kernalFree
        | not (useKernal subs) = Set.fromList [0x90 .. 0xFA]
        | otherwise            = cassetteFree <> rs232Free <> sharedFree <> irqFree

    -- Addresses used exclusively by the cassette/tape subsystem.
    -- $92, $96, $9B-$9C, $9E-$9F, $A3-$A6, $B0-$B3, $B6, $BE-$BF, $C0
    cassetteFree :: Set Word8
    cassetteFree
        | useCassette subs = Set.empty
        | otherwise        = Set.fromList cassetteOnlyAddrs

    cassetteOnlyAddrs :: [Word8]
    cassetteOnlyAddrs =
        [ 0x92, 0x96
        , 0x9B, 0x9C, 0x9E, 0x9F
        , 0xA3, 0xA4, 0xA5, 0xA6
        , 0xB0, 0xB1, 0xB2, 0xB3, 0xB6
        , 0xBE, 0xBF, 0xC0
        ]

    -- Addresses used exclusively by RS-232.
    -- $A9-$AB, $F7-$FA
    rs232Free :: Set Word8
    rs232Free
        | useRS232 subs = Set.empty
        | otherwise     = Set.fromList rs232OnlyAddrs

    rs232OnlyAddrs :: [Word8]
    rs232OnlyAddrs =
        [ 0xA9, 0xAA, 0xAB
        , 0xF7, 0xF8, 0xF9, 0xFA
        ]

    -- Addresses shared between cassette and RS-232.
    -- Only free when BOTH subsystems are off.
    -- $A7, $A8, $B4, $B5, $BD
    sharedFree :: Set Word8
    sharedFree
        | useCassette subs || useRS232 subs = Set.empty
        | otherwise = Set.fromList [0xA7, 0xA8, 0xB4, 0xB5, 0xBD]

    -- Addresses used by the default KERNAL IRQ handler
    -- (jiffy clock, keyboard scan, cursor blink, screen editor).
    -- $A0-$A2, $C5-$F6
    irqFree :: Set Word8
    irqFree
        | useKernalIRQ subs = Set.empty
        | otherwise         = Set.fromList $ [0xA0 .. 0xA2] ++ [0xC5 .. 0xF6]

-- | Load C64 ROM images into emulator memory based on 'TargetConfig' paths.
-- Skips any ROM whose path is 'Nothing'.
loadC64Roms :: TargetConfig -> CPUState -> IO CPUState
loadC64Roms cfg s0 = do
    s1 <- loadRom (basicRom   cfg) basicROM   s0
    s2 <- loadRom (chargenRom cfg) chargenROM s1
    loadRom (kernalRom cfg) kernalROM s2
  where
    loadRom Nothing     _    s = pure s
    loadRom (Just path) addr s = do
        bs <- BS.unpack <$> BS.readFile path
        pure $ over mem (loadBytes addr bs) s
