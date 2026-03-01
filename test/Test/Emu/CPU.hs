module Test.Emu.CPU (tests) where

import Data.Bits (testBit)
import Data.Word (Word8, Word16)
import Test.QuickCheck

import Test.Helpers (check, section)
import Emu.CPU

tests :: [IO Bool]
tests =
    [ section "Emu.CPU"
    -- Initial state
    , check "initCPU A=0"      $ view regA  initCPU == 0
    , check "initCPU X=0"      $ view regX  initCPU == 0
    , check "initCPU Y=0"      $ view regY  initCPU == 0
    , check "initCPU SP=0xFD"  $ view regSP initCPU == 0xFD
    , check "initCPU P=0x24"   $ view regP  initCPU == 0x24
    , check "initCPU PC=0"     $ view regPC initCPU == 0
    , check "initCPU cycles=0" $ view cycles initCPU == 0
    -- Lens laws: get-set, set-get, set-set for register lenses
    , check "regA get-set"  $ prop_getSet regA
    , check "regA set-get"  $ prop_setGet regA
    , check "regA set-set"  $ prop_setSet regA
    , check "regX get-set"  $ prop_getSet regX
    , check "regX set-get"  $ prop_setGet regX
    , check "regY get-set"  $ prop_getSet regY
    , check "regY set-get"  $ prop_setGet regY
    , check "regSP get-set" $ prop_getSet regSP
    , check "regSP set-get" $ prop_setGet regSP
    , check "regP get-set"  $ prop_getSet regP
    , check "regP set-get"  $ prop_setGet regP
    , check "regPC get-set" $ prop_getSet_w16 regPC
    , check "regPC set-get" $ prop_setGet_w16 regPC
    , check "regPC set-set" $ prop_setSet_w16 regPC
    -- Flag bit positions
    , check "flagC is bit 0" $ prop_flagBit flagC 0
    , check "flagZ is bit 1" $ prop_flagBit flagZ 1
    , check "flagI is bit 2" $ prop_flagBit flagI 2
    , check "flagD is bit 3" $ prop_flagBit flagD 3
    , check "flagB is bit 4" $ prop_flagBit flagB 4
    , check "flagV is bit 6" $ prop_flagBit flagV 6
    , check "flagN is bit 7" $ prop_flagBit flagN 7
    -- Flag independence
    , check "setting flagC doesn't affect flagN" $ prop_flagIndependence flagC flagN
    , check "setting flagZ doesn't affect flagV" $ prop_flagIndependence flagZ flagV
    , check "setting flagI doesn't affect flagD" $ prop_flagIndependence flagI flagD
    -- updateNZ
    , check "updateNZ zero sets Z, clears N"    prop_updateNZ_zero
    , check "updateNZ 0x80 sets N, clears Z"    prop_updateNZ_neg
    , check "updateNZ positive clears both"      prop_updateNZ_pos
    -- memAt
    , check "memAt write-read"    prop_memAt_writeRead
    , check "memAt isolation"     prop_memAt_isolation
    ]

-- Lens law helpers for Word8 lenses
prop_getSet :: Lens' CPUState Word8 -> Word8 -> Bool
prop_getSet l v = view l (set l v initCPU) == v

prop_setGet :: Lens' CPUState Word8 -> Bool
prop_setGet l = set l (view l initCPU) initCPU == initCPU

prop_setSet :: Lens' CPUState Word8 -> Word8 -> Word8 -> Bool
prop_setSet l a b = set l b (set l a initCPU) == set l b initCPU

-- Lens law helpers for Word16 lenses
prop_getSet_w16 :: Lens' CPUState Word16 -> Word16 -> Bool
prop_getSet_w16 l v = view l (set l v initCPU) == v

prop_setGet_w16 :: Lens' CPUState Word16 -> Bool
prop_setGet_w16 l = set l (view l initCPU) initCPU == initCPU

prop_setSet_w16 :: Lens' CPUState Word16 -> Word16 -> Word16 -> Bool
prop_setSet_w16 l a b = set l b (set l a initCPU) == set l b initCPU

-- Flag bit position: setting flag should affect only the expected bit
prop_flagBit :: Lens' CPUState Bool -> Int -> Bool
prop_flagBit fl bit =
    let s0 = set regP 0x00 initCPU
        s1 = set fl True s0
    in  testBit (view regP s1) bit && view fl s1

-- Flag independence: setting one flag doesn't affect another
prop_flagIndependence :: Lens' CPUState Bool -> Lens' CPUState Bool -> Bool -> Bool
prop_flagIndependence f1 f2 v =
    let s0  = set regP 0x00 initCPU
        s1  = set f2 True s0       -- set f2 first
        s2  = set f1 v s1          -- then change f1
    in  view f2 s2 == True          -- f2 still True

prop_updateNZ_zero :: Bool
prop_updateNZ_zero =
    let s = updateNZ 0 initCPU
    in  view flagZ s && not (view flagN s)

prop_updateNZ_neg :: Bool
prop_updateNZ_neg =
    let s = updateNZ 0x80 initCPU
    in  not (view flagZ s) && view flagN s

prop_updateNZ_pos :: Word8 -> Property
prop_updateNZ_pos v =
    v > 0 && v < 0x80 ==>
    let s = updateNZ v initCPU
    in  not (view flagZ s) && not (view flagN s)

prop_memAt_writeRead :: Word16 -> Word8 -> Bool
prop_memAt_writeRead addr val =
    view (memAt addr) (set (memAt addr) val initCPU) == val

prop_memAt_isolation :: Word16 -> Word16 -> Word8 -> Property
prop_memAt_isolation a b val =
    a /= b ==> view (memAt b) (set (memAt a) val initCPU) == 0
