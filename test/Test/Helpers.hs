{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Helpers
    ( -- * Test runner helpers
      detArgs, check, checkWith, checkOnce, checkIO, section
      -- * Assembly helpers
    , simpleConfig, asm, asmAt, zpConfig, asmZP
      -- * TestInsn for random program generation
    , TestInsn(..), testInsnBytes
      -- * Re-exports for convenience
    , isLeft
    ) where

import Data.Set qualified as Set
import Data.Word (Word8, Word16)
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import Asm.Monad (ASM, TargetConfig(..), assemble)
import Asm.Mos6502 (Imm(..), ZP(..), ZPX(..), ZPY(..), Abs(..), AbsX(..), AbsY(..), Ind(..), IndX(..), IndY(..))
import Target.C64 (c64TargetConfig, defaultC64Subsystems, C64Subsystems(..))
import ISA.Mos6502 (Instruction(..), decode, encode)

-- ---------------------------------------------------------------------------
-- Assembly helpers
-- ---------------------------------------------------------------------------

simpleConfig :: Word16 -> TargetConfig
simpleConfig org = TargetConfig { origin = org, freeZeroPage = Set.empty
                               , kernalRom = Nothing, basicRom = Nothing, chargenRom = Nothing }

asm :: ASM a -> [Word8]
asm = snd . assemble (simpleConfig 0x0000)

asmAt :: Word16 -> ASM a -> [Word8]
asmAt org = snd . assemble (simpleConfig org)

zpConfig :: [Word8] -> TargetConfig
zpConfig addrs = TargetConfig { origin = 0x0000, freeZeroPage = Set.fromList addrs
                             , kernalRom = Nothing, basicRom = Nothing, chargenRom = Nothing }

asmZP :: ASM a -> [Word8]
asmZP = snd . assemble (c64TargetConfig 0x0800 defaultC64Subsystems { useBasic = False })

-- ---------------------------------------------------------------------------
-- TestInsn: random valid 6502 instructions
-- ---------------------------------------------------------------------------

newtype TestInsn = TestInsn Instruction
    deriving (Show, Eq)

testInsnBytes :: TestInsn -> [Word8]
testInsnBytes (TestInsn instr) = encode instr

instance Arbitrary TestInsn where
    arbitrary = do
        let entries = [ (n, f) | i <- [0x00..0xFF :: Word8]
                      , Just (n, f) <- [decode i] ]
        (_n, f) <- elements entries
        b1 <- arbitrary
        b2 <- arbitrary
        pure (TestInsn (f b1 b2))

-- ---------------------------------------------------------------------------
-- Arbitrary instances for operand newtypes
-- ---------------------------------------------------------------------------

instance Arbitrary Imm  where arbitrary = Imm  <$> arbitrary
instance Arbitrary ZP   where arbitrary = ZP   <$> arbitrary
instance Arbitrary ZPX  where arbitrary = ZPX  <$> arbitrary
instance Arbitrary ZPY  where arbitrary = ZPY  <$> arbitrary
instance Arbitrary Abs  where arbitrary = Abs  <$> arbitrary
instance Arbitrary AbsX where arbitrary = AbsX <$> arbitrary
instance Arbitrary AbsY where arbitrary = AbsY <$> arbitrary
instance Arbitrary Ind  where arbitrary = Ind  <$> arbitrary
instance Arbitrary IndX where arbitrary = IndX <$> arbitrary
instance Arbitrary IndY where arbitrary = IndY <$> arbitrary

-- ---------------------------------------------------------------------------
-- Test runner helpers
-- ---------------------------------------------------------------------------

detArgs :: Args
detArgs = stdArgs { maxSuccess = 1000, replay = Just (mkQCGen 42, 0) }

check :: Testable prop => String -> prop -> IO Bool
check = checkWith detArgs

checkWith :: Testable prop => Args -> String -> prop -> IO Bool
checkWith args name prop = do
    putStr $ "  " ++ name ++ ": "
    r <- quickCheckWithResult args prop
    pure $ isSuccess r

checkOnce :: Testable prop => String -> prop -> IO Bool
checkOnce = checkWith detArgs { maxSuccess = 1 }

checkIO :: String -> IO Bool -> IO Bool
checkIO name action = do
    putStr $ "  " ++ name ++ ": "
    ok <- action
    putStrLn $ if ok then "+++ OK" else "*** FAILED"
    pure ok

section :: String -> IO Bool
section name = do
    putStrLn $ "\n=== " ++ name ++ " ==="
    pure True

-- ---------------------------------------------------------------------------
-- Misc helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
