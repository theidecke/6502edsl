module Target.C64.D64
    ( toD64
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, popCount)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8, Word32)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Number of sectors on each track (1-based index; index 0 unused).
sectorsPerTrack :: [Int]
sectorsPerTrack =
    [0]             -- index 0: unused (tracks are 1-based)
    ++ replicate 17 21   -- tracks  1-17: 21 sectors
    ++ replicate  7 19   -- tracks 18-24: 19 sectors
    ++ replicate  6 18   -- tracks 25-30: 18 sectors
    ++ replicate  5 17   -- tracks 31-35: 17 sectors

sectorSize :: Int
sectorSize = 256

dataPerSector :: Int
dataPerSector = 254

totalSectors :: Int
totalSectors = 683

dirTrack :: Int
dirTrack = 18

-- | Maximum data sectors available (all 683 minus BAM + directory on track 18).
maxDataSectors :: Int
maxDataSectors = totalSectors - sectorsPerTrack !! dirTrack

-- ---------------------------------------------------------------------------
-- Sector addressing
-- ---------------------------------------------------------------------------

type TrackSector = (Int, Int)

-- | All (track, sector) pairs in disk order.
allSectors :: [TrackSector]
allSectors =
    [ (t, s)
    | t <- [1..35]
    , s <- [0 .. sectorsPerTrack !! t - 1]
    ]

-- ---------------------------------------------------------------------------
-- Sector allocation
-- ---------------------------------------------------------------------------

-- | Allocate data sectors sequentially, skipping the directory track.
allocSectors :: Int -> [TrackSector]
allocSectors n = take n
    [ (t, s)
    | t <- [1..35]
    , t /= dirTrack
    , s <- [0 .. sectorsPerTrack !! t - 1]
    ]

-- ---------------------------------------------------------------------------
-- Data chunking and sector building
-- ---------------------------------------------------------------------------

chunksOf254 :: [Word8] -> [[Word8]]
chunksOf254 [] = []
chunksOf254 xs =
    let (chunk, rest) = splitAt dataPerSector xs
    in  chunk : chunksOf254 rest

-- | Build data sectors with T/S chain links.
-- Each sector is exactly 256 bytes: 2-byte link + up to 254 data bytes.
buildDataSectors :: [TrackSector] -> [[Word8]] -> Map TrackSector [Word8]
buildDataSectors locations chunks = Map.fromList (zipWith3 mkSector locations nextLinks chunks)
  where
    nextLinks :: [Maybe TrackSector]
    nextLinks = map Just (drop 1 locations) ++ [Nothing]

    mkSector :: TrackSector -> Maybe TrackSector -> [Word8] -> (TrackSector, [Word8])
    mkSector loc mnext chunk =
        let (linkT, linkS) = case mnext of
                Just (nt, ns) -> (fromIntegral nt, fromIntegral ns)
                Nothing       -> (0x00, fromIntegral (length chunk + 1))
            padded = chunk ++ replicate (dataPerSector - length chunk) 0x00
        in  (loc, linkT : linkS : padded)

-- ---------------------------------------------------------------------------
-- PETSCII conversion
-- ---------------------------------------------------------------------------

-- | Simple ASCII to PETSCII mapping (uppercase mode).
toPETSCII :: Char -> Word8
toPETSCII c
    | c >= 'A' && c <= 'Z' = fromIntegral (fromEnum c)  -- $41-$5A (same as ASCII)
    | c >= 'a' && c <= 'z' = fromIntegral (fromEnum c - 32)  -- lowercase -> uppercase
    | c >= '0' && c <= '9' = fromIntegral (fromEnum c)
    | c == ' '             = 0x20
    | otherwise            = 0x20  -- fallback to space

-- | Convert and pad a filename to exactly 16 PETSCII bytes (padded with $A0).
petsciiFilename :: String -> [Word8]
petsciiFilename name =
    let converted = map toPETSCII (take 16 name)
    in  converted ++ replicate (16 - length converted) 0xA0

-- ---------------------------------------------------------------------------
-- BAM (Block Availability Map) — track 18, sector 0
-- ---------------------------------------------------------------------------

buildBAM :: [TrackSector] -> String -> [Word8]
buildBAM usedSectors diskName =
    let bam = concat
            [ [18, 1]             -- directory pointer: track 18, sector 1
            , [0x41]              -- DOS version: 'A'
            , [0x00]              -- double-sided flag (unused on 1541)
            , concatMap bamEntry [1..35]  -- 4 bytes per track = 140 bytes
            , petsciiFilename diskName    -- disk name (16 bytes)
            , [0xA0, 0xA0]       -- padding after disk name
            , [0x32, 0x41]       -- disk ID "2A"
            , [0xA0]             -- padding
            , [0x32, 0x41]       -- DOS type "2A"
            , [0xA0, 0xA0, 0xA0, 0xA0]  -- padding
            ]
    in  bam ++ replicate (sectorSize - length bam) 0x00
  where
    -- Set of all used (track, sector) pairs for efficient lookup
    usedSet :: Map TrackSector ()
    usedSet = Map.fromList [(ts, ()) | ts <- usedSectors ++ [(dirTrack, 0), (dirTrack, 1)]]

    bamEntry :: Int -> [Word8]
    bamEntry t =
        let numSectors = sectorsPerTrack !! t
            -- Build bitmap: bit i = 1 means sector i is free
            bitmap :: Word32
            bitmap = foldl setBitIfFree 0 [0 .. numSectors - 1]
            setBitIfFree :: Word32 -> Int -> Word32
            setBitIfFree bm i
                | Map.member (t, i) usedSet = bm
                | otherwise                 = bm .|. (1 `shiftL` i)
            freeCount = popCount bitmap
        in  [ fromIntegral freeCount
            , fromIntegral (bitmap .&. 0xFF)
            , fromIntegral ((bitmap `shiftR` 8) .&. 0xFF)
            , fromIntegral ((bitmap `shiftR` 16) .&. 0xFF)
            ]

-- ---------------------------------------------------------------------------
-- Directory — track 18, sector 1
-- ---------------------------------------------------------------------------

buildDirectory :: TrackSector -> String -> Int -> [Word8]
buildDirectory firstDataSector fileName fileSectors =
    let (firstT, firstS) = firstDataSector
        entry = concat
            [ [0x00, 0xFF]                        -- next dir T/S (no more dir sectors)
            , [0x82]                              -- file type: closed PRG
            , [fromIntegral firstT, fromIntegral firstS]  -- first data T/S
            , petsciiFilename fileName            -- filename (16 bytes)
            , replicate 9 0x00                    -- REL side-sector pointer + record length + unused
            , [lo16 fileSectors, hi16 fileSectors]  -- file size in sectors (little-endian)
            ]
    in  entry ++ replicate (sectorSize - length entry) 0x00
  where
    lo16 n = fromIntegral (n .&. 0xFF)
    hi16 n = fromIntegral ((n `shiftR` 8) .&. 0xFF)

-- | Build a directory for an empty file (no data sectors).
buildEmptyDirectory :: String -> [Word8]
buildEmptyDirectory fileName =
    let entry = concat
            [ [0x00, 0xFF]                        -- next dir T/S
            , [0x82]                              -- file type: closed PRG
            , [0x00, 0x00]                        -- no data sectors
            , petsciiFilename fileName
            , replicate 9 0x00
            , [0x00, 0x00]                        -- 0 sectors
            ]
    in  entry ++ replicate (sectorSize - length entry) 0x00

-- ---------------------------------------------------------------------------
-- Image assembly
-- ---------------------------------------------------------------------------

-- | Generate a complete D64 disk image containing a single PRG file.
--
-- Takes a filename (max 16 chars, ASCII) and PRG content (including the
-- 2-byte load address header). Returns a 174,848-byte D64 image.
toD64 :: String -> [Word8] -> [Word8]
toD64 fileName prgData
    | numDataSectors > maxDataSectors =
        error $ "D64: PRG data too large — needs " ++ show numDataSectors
             ++ " sectors, max " ++ show maxDataSectors
    | null prgData =
        buildImage Map.empty (buildBAM [] fileName) (buildEmptyDirectory fileName)
    | otherwise =
        case allocSectors numDataSectors of
            [] -> error "D64: allocSectors returned empty list"
            locations@(firstSec:_) ->
                let chunks    = chunksOf254 prgData
                    dataSects = buildDataSectors locations chunks
                    bam       = buildBAM locations fileName
                    dir       = buildDirectory firstSec fileName numDataSectors
                in  buildImage dataSects bam dir
  where
    numDataSectors :: Int
    numDataSectors
        | null prgData = 0
        | otherwise    = (length prgData + dataPerSector - 1) `div` dataPerSector

buildImage :: Map TrackSector [Word8] -> [Word8] -> [Word8] -> [Word8]
buildImage dataSectors bamBytes dirBytes =
    concatMap sectorContent allSectors
  where
    specialSectors :: Map TrackSector [Word8]
    specialSectors = Map.fromList
        [ ((dirTrack, 0), bamBytes)
        , ((dirTrack, 1), dirBytes)
        ]

    sectorContent :: TrackSector -> [Word8]
    sectorContent ts = case Map.lookup ts specialSectors of
        Just bytes -> bytes
        Nothing    -> case Map.lookup ts dataSectors of
            Just bytes -> bytes
            Nothing    -> replicate sectorSize 0x00
