module Test.Target (tests) where

import Data.Char (ord)
import Data.Map.Strict qualified as Map
import Data.Word (Word8, Word16)
import Test.QuickCheck

import Asm.Monad (lo, hi)
import Backend.C64.D64 (toD64)
import Backend.C64.PRG (toPRG)
import Backend.C64.ViceLabels (exportViceLabels)
import Target.C64.Data (byte, word, petscii, pstring, charToPetscii)
import Test.Helpers

-- ---------------------------------------------------------------------------
-- PRG generation (3 props)
-- ---------------------------------------------------------------------------

prop_prgLength :: Word16 -> [Word8] -> Property
prop_prgLength addr bs =
    length bs < 1000 ==>
        length (toPRG addr bs) == length bs + 2

prop_prgHeader :: Word16 -> Bool
prop_prgHeader addr =
    let prg = toPRG addr [0x42]
    in  prg !! 0 == lo addr && prg !! 1 == hi addr

prop_prgPayload :: Word16 -> [Word8] -> Property
prop_prgPayload addr bs =
    length bs < 1000 ==>
        drop 2 (toPRG addr bs) == bs

-- ---------------------------------------------------------------------------
-- D64 generation (5 props)
-- ---------------------------------------------------------------------------

prop_d64ImageSize :: [Word8] -> Property
prop_d64ImageSize bs =
    length bs < 5000 ==>
        let prg = toPRG 0x0801 bs
        in  length (toD64 "TEST" prg) == 174848

prop_d64BamHeader :: Bool
prop_d64BamHeader =
    let img = toD64 "TEST" [0x01, 0x08]
        bamOffset = 91392
    in  img !! bamOffset == 18
     && img !! (bamOffset + 1) == 1
     && img !! (bamOffset + 2) == 0x41

prop_d64DirectoryFileType :: Bool
prop_d64DirectoryFileType =
    let img = toD64 "TEST" [0x01, 0x08, 0xEA]
        dirOffset = 91648
    in  img !! (dirOffset + 2) == 0x82

prop_d64RoundTrip :: [Word8] -> Property
prop_d64RoundTrip payload =
    length payload > 0 && length payload < 2000 ==>
        let prg = toPRG 0x0801 payload
            img = toD64 "TEST" prg
            extracted = d64Extract img
        in  extracted == prg

prop_d64RoundTripLarge :: Bool
prop_d64RoundTripLarge =
    let payload = [fromIntegral (i `mod` 251) :: Word8 | i <- [0 :: Int .. 30719]]
        prg = toPRG 0x0801 payload
        img = toD64 "TEST" prg
        extracted = d64Extract img
    in  extracted == prg

-- | Extract the first file's PRG data from a D64 image by following the T/S chain.
d64Extract :: [Word8] -> [Word8]
d64Extract img =
    let dirOffset = 91648
        firstTrack  = fromIntegral (img !! (dirOffset + 3)) :: Int
        firstSector = fromIntegral (img !! (dirOffset + 4)) :: Int
    in  followChain img (firstTrack, firstSector)

tsOffset :: (Int, Int) -> Int
tsOffset (t, s) =
    let spt = [0] ++ replicate 17 21 ++ replicate 7 19
                   ++ replicate 6 18 ++ replicate 5 17
    in  (sum (take t spt) + s) * 256

followChain :: [Word8] -> (Int, Int) -> [Word8]
followChain img (t, s)
    | t == 0    = []
    | otherwise =
        let off = tsOffset (t, s)
            nextT = fromIntegral (img !! off) :: Int
            nextS = fromIntegral (img !! (off + 1)) :: Int
        in  if nextT == 0
            then take (nextS - 1) (drop (off + 2) img)
            else take 254 (drop (off + 2) img) ++ followChain img (nextT, nextS)

-- ---------------------------------------------------------------------------
-- Data embedding (12 props)
-- ---------------------------------------------------------------------------

prop_byteIdentity :: [Word8] -> Property
prop_byteIdentity bs = length bs < 256 ==> asm (byte bs) == bs

prop_wordLE :: Word16 -> Bool
prop_wordLE w = asm (word [w]) == [lo w, hi w]

prop_wordMultiple :: Bool
prop_wordMultiple = asm (word [0x1234, 0x5678]) == [0x34, 0x12, 0x78, 0x56]

prop_wordEmpty :: Bool
prop_wordEmpty = asm (word []) == []

prop_petsciiUppercase :: Bool
prop_petsciiUppercase = asm (petscii "HELLO") == [0x48, 0x45, 0x4C, 0x4C, 0x4F]

prop_petsciiLowercase :: Bool
prop_petsciiLowercase = asm (petscii "ab") == [0xC1, 0xC2]

prop_petsciiDigits :: Bool
prop_petsciiDigits = asm (petscii "09") == [0x30, 0x39]

prop_petsciiSpace :: Bool
prop_petsciiSpace = asm (petscii " ") == [0x20]

prop_pstringNullTerminated :: Bool
prop_pstringNullTerminated = asm (pstring "HI") == [0x48, 0x49, 0x00]

prop_charToPetsciiUpperRange :: Bool
prop_charToPetsciiUpperRange =
    all (\c -> charToPetscii c == fromIntegral (ord c)) ['A'..'Z']

prop_charToPetsciiLowerRange :: Bool
prop_charToPetsciiLowerRange =
    all (\c -> charToPetscii c == fromIntegral (ord c - ord 'a' + 0xC1)) ['a'..'z']

prop_charToPetsciiPunctuation :: Bool
prop_charToPetsciiPunctuation =
    charToPetscii '@' == 0x40
    && charToPetscii '[' == 0x5B
    && charToPetscii '!' == 0x21

-- ---------------------------------------------------------------------------
-- VICE label export (3 props)
-- ---------------------------------------------------------------------------

prop_viceLabelsFormat :: Bool
prop_viceLabelsFormat =
    exportViceLabels (Map.fromList [(0x0810, ["main"]), (0x0820, ["loop"])])
    == "al C:0810 .main\nal C:0820 .loop\n"

prop_viceLabelsEmpty :: Bool
prop_viceLabelsEmpty = exportViceLabels Map.empty == ""

prop_viceLabelsLeadingZeros :: Bool
prop_viceLabelsLeadingZeros =
    exportViceLabels (Map.fromList [(0x0010, ["start"])]) == "al C:0010 .start\n"

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "PRG generation"
    , check "prg length = input + 2"   prop_prgLength
    , check "prg header matches lo/hi" prop_prgHeader
    , check "prg payload preserved"    prop_prgPayload

    , section "D64 generation"
    , check "d64 image size = 174848"  prop_d64ImageSize
    , checkOnce "d64 BAM header bytes"     prop_d64BamHeader
    , checkOnce "d64 directory file type"  prop_d64DirectoryFileType
    , check "d64 round-trip T/S chain" prop_d64RoundTrip
    , checkOnce "d64 round-trip large (~30KB)" prop_d64RoundTripLarge

    , section "Data embedding"
    , check "byte identity"                 prop_byteIdentity
    , check "word little-endian"            prop_wordLE
    , checkOnce "word multiple values"      prop_wordMultiple
    , checkOnce "word empty list"           prop_wordEmpty
    , checkOnce "petscii uppercase"         prop_petsciiUppercase
    , checkOnce "petscii lowercase"         prop_petsciiLowercase
    , checkOnce "petscii digits"            prop_petsciiDigits
    , checkOnce "petscii space"             prop_petsciiSpace
    , checkOnce "pstring null-terminated"   prop_pstringNullTerminated
    , checkOnce "charToPetscii A-Z"         prop_charToPetsciiUpperRange
    , checkOnce "charToPetscii a-z"         prop_charToPetsciiLowerRange
    , checkOnce "charToPetscii punctuation" prop_charToPetsciiPunctuation

    , section "VICE label export"
    , checkOnce "format check"              prop_viceLabelsFormat
    , checkOnce "empty list"                prop_viceLabelsEmpty
    , checkOnce "leading zeros"             prop_viceLabelsLeadingZeros
    ]
