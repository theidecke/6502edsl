module Test.Emu.Mem (tests) where

import Data.List (sortOn)
import Data.Word (Word8, Word16)
import Test.QuickCheck

import Test.Helpers (check, section)
import Emu.Mem

tests :: [IO Bool]
tests =
    [ section "Emu.Mem"
    , check "empty reads zero"          prop_emptyReadsZero
    , check "write-read identity"       prop_writeRead
    , check "write to a doesn't affect b" prop_writeIsolation
    , check "last write wins"           prop_lastWriteWins
    , check "loadBytes roundtrip"       prop_loadBytesRoundtrip
    , check "persistence (old Mem unchanged)" prop_persistence
    , section "Emu.Mem (diffMem)"
    , check "diffMem identical"         prop_diffMemIdentical
    , check "diffMem one write"         prop_diffMemOneWrite
    , check "diffMem symmetric"         prop_diffMemSymmetric
    ]

prop_emptyReadsZero :: Word16 -> Bool
prop_emptyReadsZero addr = readByte addr emptyMem == 0

prop_writeRead :: Word16 -> Word8 -> Bool
prop_writeRead addr val = readByte addr (writeByte addr val emptyMem) == val

prop_writeIsolation :: Word16 -> Word16 -> Word8 -> Property
prop_writeIsolation a b val =
    a /= b ==> readByte b (writeByte a val emptyMem) == 0

prop_lastWriteWins :: Word16 -> Word8 -> Word8 -> Bool
prop_lastWriteWins addr v1 v2 =
    readByte addr (writeByte addr v2 (writeByte addr v1 emptyMem)) == v2

prop_loadBytesRoundtrip :: Word16 -> [Word8] -> Property
prop_loadBytesRoundtrip base bs =
    length bs <= 256 ==>
    let m = loadBytes base bs emptyMem
    in  all (\(i, v) -> readByte (base + fromIntegral i) m == v)
            (zip [0 :: Int ..] bs)

prop_persistence :: Word16 -> Word8 -> Bool
prop_persistence addr val =
    let m0 = emptyMem
        _m1 = writeByte addr val m0
    in  readByte addr m0 == 0  -- original unchanged

prop_diffMemIdentical :: Word16 -> Word8 -> Bool
prop_diffMemIdentical addr val =
    let m = writeByte addr val emptyMem
    in  diffMem m m == []

prop_diffMemOneWrite :: Word16 -> Word8 -> Property
prop_diffMemOneWrite addr val =
    val /= 0 ==>
    let m0 = emptyMem
        m1 = writeByte addr val m0
    in  diffMem m0 m1 == [(addr, 0, val)]

prop_diffMemSymmetric :: Word16 -> Word8 -> Property
prop_diffMemSymmetric addr val =
    val /= 0 ==>
    let m0 = emptyMem
        m1 = writeByte addr val m0
        fwd = diffMem m0 m1
        rev = diffMem m1 m0
    in  sortOn (\(a,_,_) -> a) (map (\(a,o,n) -> (a,n,o)) rev) ==
        sortOn (\(a,_,_) -> a) fwd
