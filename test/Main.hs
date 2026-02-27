{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8, Word16)
import System.Exit (exitFailure)
import Test.QuickCheck hiding ((.&.), label)
import Test.QuickCheck.Random (mkQCGen)

import Asm.Monad (ASM, TargetConfig(..), assemble, emit, label, allocZP, lo, hi)
import Asm.Mos6502
import ISA.Mos6502 (Opcode(..))
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Expected operand byte count for each addressing mode.
modeSize :: Mode -> Int
modeSize MImplied     = 1
modeSize MAccumulator = 1
modeSize MImmediate   = 2
modeSize MZeroPage    = 2
modeSize MZeroPageX   = 2
modeSize MZeroPageY   = 2
modeSize MAbsolute    = 3
modeSize MAbsoluteX   = 3
modeSize MAbsoluteY   = 3
modeSize MIndirect    = 3
modeSize MIndirectX   = 2
modeSize MIndirectY   = 2
modeSize MRelative    = 2

-- | Number of operand bytes (instruction size minus opcode byte).
operandSize :: Mode -> Int
operandSize m = modeSize m - 1

-- | Minimal config with no free zero-page bytes.
simpleConfig :: Word16 -> TargetConfig
simpleConfig org = TargetConfig { origin = org, freeZeroPage = Set.empty }

-- | Assemble a single ASM action and return the emitted bytes.
asm :: ASM a -> [Word8]
asm = snd . assemble (simpleConfig 0x0000)

-- | Assemble starting at a given origin.
asmAt :: Word16 -> ASM a -> [Word8]
asmAt org = snd . assemble (simpleConfig org)

-- | Config with specific free zero-page bytes.
zpConfig :: [Word8] -> TargetConfig
zpConfig addrs = TargetConfig { origin = 0x0000, freeZeroPage = Set.fromList addrs }

-- ---------------------------------------------------------------------------
-- Test instruction type (for random program generation)
-- ---------------------------------------------------------------------------

data TestInsn = TestInsn Opcode Mode [Word8]
    deriving (Show)

testInsnBytes :: TestInsn -> [Word8]
testInsnBytes (TestInsn opc mode operands) =
    opcodeFor opc mode : operands

instance Arbitrary TestInsn where
    arbitrary = do
        let keys = Map.keys opcodeTable
        (opc, mode) <- elements keys
        operands <- vectorOf (operandSize mode) arbitrary
        pure (TestInsn opc mode operands)

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
-- lo/hi byte helpers (3 props)
-- ---------------------------------------------------------------------------

prop_loMasks :: Word16 -> Bool
prop_loMasks w = lo w == fromIntegral (w .&. 0xFF)

prop_hiMasks :: Word16 -> Bool
prop_hiMasks w = hi w == fromIntegral (w `shiftR` 8)

prop_lohiRoundtrip :: Word16 -> Bool
prop_lohiRoundtrip w =
    (fromIntegral (hi w) `shiftL` 8 .|. fromIntegral (lo w) :: Word16) == w

-- ---------------------------------------------------------------------------
-- Opcode table integrity (3 props)
-- ---------------------------------------------------------------------------

prop_opcodeTableSize :: Bool
prop_opcodeTableSize = Map.size opcodeTable == 151

prop_opcodeTableInjective :: Bool
prop_opcodeTableInjective =
    let vals = Map.elems opcodeTable
    in  length (nub vals) == length vals

prop_opcodeTableAllOpcodes :: Bool
prop_opcodeTableAllOpcodes =
    let opcodes = nub [opc | (opc, _) <- Map.keys opcodeTable]
    in  length opcodes == length [minBound .. maxBound :: Opcode]

-- ---------------------------------------------------------------------------
-- Instruction size determinism (2 props) — critical for MonadFix
-- ---------------------------------------------------------------------------

prop_modeSizeCorrect :: TestInsn -> Bool
prop_modeSizeCorrect ti@(TestInsn _ mode _) =
    length (asm (emit (testInsnBytes ti))) == modeSize mode

prop_sizeDeterministic :: TestInsn -> TestInsn -> Property
prop_sizeDeterministic (TestInsn _ m1 _) (TestInsn _ m2 _) =
    m1 == m2 ==> modeSize m1 == modeSize m2

-- ---------------------------------------------------------------------------
-- Instruction byte correctness (2 props)
-- ---------------------------------------------------------------------------

prop_firstByteIsOpcode :: TestInsn -> Bool
prop_firstByteIsOpcode ti@(TestInsn opc mode _) =
    case asm (emit (testInsnBytes ti)) of
        (b:_) -> b == opcodeFor opc mode
        []    -> False

prop_operandBytesPreserved :: TestInsn -> Bool
prop_operandBytesPreserved ti@(TestInsn _ _ operands) =
    drop 1 (asm (emit (testInsnBytes ti))) == operands

-- ---------------------------------------------------------------------------
-- EDSL instruction functions (~9 props)
-- ---------------------------------------------------------------------------

prop_ldaImm :: Imm -> Bool
prop_ldaImm v@(Imm b) = asm (lda v) == [opcodeFor LDA MImmediate, b]

prop_ldaZP :: ZP -> Bool
prop_ldaZP v@(ZP b) = asm (lda v) == [opcodeFor LDA MZeroPage, b]

prop_ldaAbs :: Abs -> Bool
prop_ldaAbs v@(Abs w) = asm (lda v) == [opcodeFor LDA MAbsolute, lo w, hi w]

prop_staZPX :: ZPX -> Bool
prop_staZPX v@(ZPX b) = asm (sta v) == [opcodeFor STA MZeroPageX, b]

prop_ldxAbsY :: AbsY -> Bool
prop_ldxAbsY v@(AbsY w) = asm (ldx v) == [opcodeFor LDX MAbsoluteY, lo w, hi w]

prop_ldaIndX :: IndX -> Bool
prop_ldaIndX v@(IndX b) = asm (lda v) == [opcodeFor LDA MIndirectX, b]

prop_ldaIndY :: IndY -> Bool
prop_ldaIndY v@(IndY b) = asm (lda v) == [opcodeFor LDA MIndirectY, b]

prop_nopImplied :: Bool
prop_nopImplied = asm nop == [opcodeFor NOP MImplied]

prop_aslAccumulator :: Bool
prop_aslAccumulator = asm asl_a == [opcodeFor ASL MAccumulator]

-- ---------------------------------------------------------------------------
-- Monad / PC tracking (4 props)
-- ---------------------------------------------------------------------------

prop_labelReturnsOrigin :: Word16 -> Bool
prop_labelReturnsOrigin org =
    let (pc, _) = assemble (simpleConfig org) label
    in  pc == org

prop_emitAdvancesPC :: Word16 -> [Word8] -> Property
prop_emitAdvancesPC org bs =
    length bs < 256 ==>
        let (pc, _) = assemble (simpleConfig org) (emit bs >> label)
        in  pc == org + fromIntegral (length bs)

prop_sequenceAdditive :: Word16 -> [Word8] -> [Word8] -> Property
prop_sequenceAdditive org a b =
    length a + length b < 256 ==>
        let (pc, _) = assemble (simpleConfig org) (emit a >> emit b >> label)
        in  pc == org + fromIntegral (length a + length b)

prop_sequenceBytesConcat :: [Word8] -> [Word8] -> Property
prop_sequenceBytesConcat a b =
    length a + length b < 256 ==>
        asm (emit a >> emit b) == a ++ b

-- ---------------------------------------------------------------------------
-- Random programs (3 props)
-- ---------------------------------------------------------------------------

prop_programLength :: [TestInsn] -> Property
prop_programLength insns =
    length insns < 100 ==>
        let bytes = asm (mapM_ (emit . testInsnBytes) insns)
        in  length bytes == sum (map (\(TestInsn _ m _) -> modeSize m) insns)

prop_assemblyDeterministic :: [TestInsn] -> Property
prop_assemblyDeterministic insns =
    length insns < 100 ==>
        let prog = mapM_ (emit . testInsnBytes) insns
        in  asm prog == asm prog

prop_programConcat :: [TestInsn] -> Property
prop_programConcat insns =
    length insns < 100 ==>
        asm (mapM_ (emit . testInsnBytes) insns) == concatMap testInsnBytes insns

-- ---------------------------------------------------------------------------
-- Branch offsets (4 props)
-- ---------------------------------------------------------------------------

prop_backwardBranch :: Bool
prop_backwardBranch =
    -- loop: DEX; BNE loop  →  [CA, D0, FD]
    let bytes = asm $ do
            loop <- label
            dex
            bne loop
    in  bytes == [0xCA, 0xD0, 0xFD]

prop_forwardBranchMdo :: Bool
prop_forwardBranchMdo =
    -- LDA $10; BEQ skip; LDA #$FF; skip: RTS  →  [A5, 10, F0, 02, A9, FF, 60]
    let bytes = asm $ mdo
            lda (ZP 0x10)
            beq skip
            lda (Imm 0xFF)
            skip <- label
            rts
    in  bytes == [0xA5, 0x10, 0xF0, 0x02, 0xA9, 0xFF, 0x60]

prop_branchOffsetBackward :: Bool
prop_branchOffsetBackward =
    -- loop: NOP; BNE loop  →  offset byte is 0xFD
    let bytes = asm $ do
            loop <- label
            nop
            bne loop
    in  bytes !! 2 == 0xFD

prop_branchOffsetForward :: Bool
prop_branchOffsetForward =
    -- BEQ skip; NOP; skip:  →  offset byte is 0x01
    let bytes = asm $ mdo
            beq skip
            nop
            skip <- label
            pure ()
    in  bytes !! 1 == 0x01

-- ---------------------------------------------------------------------------
-- Zero-page allocation (4 props)
-- ---------------------------------------------------------------------------

prop_allocZPSingleByte :: Bool
prop_allocZPSingleByte =
    let cfg = zpConfig [0x02, 0xFB, 0xFC]
        (addr, _) = assemble cfg (allocZP 1)
    in  addr == 0x02  -- smallest free byte

prop_allocZPMultiByte :: Bool
prop_allocZPMultiByte =
    -- Needs 3 contiguous bytes; [0x02] is isolated, [0xFB..0xFE] has 4 contiguous
    let cfg = zpConfig ([0x02] ++ [0xFB .. 0xFE])
        (addr, _) = assemble cfg (allocZP 3)
    in  addr == 0xFB

prop_allocZPNoOverlap :: Bool
prop_allocZPNoOverlap =
    let cfg = zpConfig [0x10 .. 0x19]  -- 10 contiguous bytes
        ((a1, a2), _) = assemble cfg $ do
            x <- allocZP 4
            y <- allocZP 4
            pure (x, y)
    in  a1 == 0x10 && a2 == 0x14

prop_allocZPInMdo :: Bool
prop_allocZPInMdo =
    -- allocZP inside mdo should work since it's an eager state transition
    let cfg = zpConfig [0x02, 0x03]
        (_, bytes) = assemble cfg $ mdo
            ptr <- allocZP 1
            lda (Imm 0x42)
            sta (ZP ptr)
            rts
    in  bytes == [ 0xA9, 0x42   -- LDA #$42
                 , 0x85, 0x02   -- STA $02
                 , 0x60         -- RTS
                 ]

-- ---------------------------------------------------------------------------
-- Addressing mode sugar (15+ props)
-- ---------------------------------------------------------------------------

-- # operator: lda # v == lda (Imm v)
prop_sugarImmediate :: Word8 -> Bool
prop_sugarImmediate v = asm (lda # v) == asm (lda (Imm v))

-- Bare Word8: lda (w :: Word8) == lda (ZP w)
prop_sugarBareWord8 :: Word8 -> Bool
prop_sugarBareWord8 w = asm (lda w) == asm (lda (ZP w))

-- Bare Word16: lda (w :: Word16) == lda (Abs w)
prop_sugarBareWord16 :: Word16 -> Bool
prop_sugarBareWord16 w = asm (lda w) == asm (lda (Abs w))

-- A_ singleton: asl A == asl_a (and lsr, rol, ror)
prop_sugarAccumulatorAsl :: Bool
prop_sugarAccumulatorAsl = asm (asl A) == asm asl_a

prop_sugarAccumulatorLsr :: Bool
prop_sugarAccumulatorLsr = asm (lsr A) == asm lsr_a

prop_sugarAccumulatorRol :: Bool
prop_sugarAccumulatorRol = asm (rol A) == asm rol_a

prop_sugarAccumulatorRor :: Bool
prop_sugarAccumulatorRor = asm (ror A) == asm ror_a

-- Tuple (Word8, X_): lda (w, X) == lda (ZPX w)
prop_sugarZPX :: Word8 -> Bool
prop_sugarZPX w = asm (lda (w, X)) == asm (lda (ZPX w))

-- Tuple (Word8, Y_): ldx (w, Y) == ldx (ZPY w)
prop_sugarZPY :: Word8 -> Bool
prop_sugarZPY w = asm (ldx (w, Y)) == asm (ldx (ZPY w))

-- Tuple (Word16, X_): lda (w, X) == lda (AbsX w)
prop_sugarAbsX :: Word16 -> Bool
prop_sugarAbsX w = asm (lda (w, X)) == asm (lda (AbsX w))

-- Tuple (Word16, Y_): lda (w, Y) == lda (AbsY w)
prop_sugarAbsY :: Word16 -> Bool
prop_sugarAbsY w = asm (lda (w, Y)) == asm (lda (AbsY w))

-- ! with Word8: lda (w ! Y) == lda (IndY w), lda (w ! X) == lda (IndX w)
prop_sugarIndY :: Word8 -> Bool
prop_sugarIndY w = asm (lda (w ! Y)) == asm (lda (IndY w))

prop_sugarIndX :: Word8 -> Bool
prop_sugarIndX w = asm (lda (w ! X)) == asm (lda (IndX w))

-- Var8 operand: allocVar8 + lda v == ZP addressing
prop_sugarVar8 :: Bool
prop_sugarVar8 =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            v <- allocVar8
            lda v
    in  bytes == [opcodeFor LDA MZeroPage, 0x10]

-- Ptr ! Y: allocPtr + lda (p ! Y) == indirect,Y
prop_sugarPtrIndY :: Bool
prop_sugarPtrIndY =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            lda (p ! Y)
    in  bytes == [opcodeFor LDA MIndirectY, 0x10]

-- Ptr arithmetic: sta (p + 1) stores to ZP address + 1
prop_sugarPtrArith :: Bool
prop_sugarPtrArith =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            sta p
            sta (p + 1)
    in  bytes == [ opcodeFor STA MZeroPage, 0x10
                 , opcodeFor STA MZeroPage, 0x11
                 ]

-- Var16 helpers: lo16/hi16 produce correct Var8 addresses
prop_sugarVar16Helpers :: Bool
prop_sugarVar16Helpers =
    let cfg = zpConfig [0x20 .. 0x29]
        (_, bytes) = assemble cfg $ do
            v <- allocVar16
            lda (lo16 v)
            lda (hi16 v)
    in  bytes == [ opcodeFor LDA MZeroPage, 0x20
                 , opcodeFor LDA MZeroPage, 0x21
                 ]

-- Typed allocators: return correct ZP addresses and don't overlap
prop_sugarAllocators :: Bool
prop_sugarAllocators =
    let cfg = zpConfig [0x02 .. 0x0A]
        ((v8, v16, p), _) = assemble cfg $ do
            a <- allocVar8   -- 1 byte at 0x02
            b <- allocVar16  -- 2 bytes at 0x03
            c <- allocPtr    -- 2 bytes at 0x05
            pure (a, b, c)
    in  v8 == Var8 0x02 && v16 == Var16 0x03 && p == Ptr 0x05

-- Var8 tuple indexed: lda (v, X) == lda (ZPX addr)
prop_sugarVar8X :: Bool
prop_sugarVar8X =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            v <- allocVar8
            lda (v, X)
    in  bytes == [opcodeFor LDA MZeroPageX, 0x10]

-- Ptr ! X: allocPtr + lda (p ! X) == indirect,X
prop_sugarPtrIndX :: Bool
prop_sugarPtrIndX =
    let cfg = zpConfig [0x10 .. 0x19]
        (_, bytes) = assemble cfg $ do
            p <- allocPtr
            lda (p ! X)
    in  bytes == [opcodeFor LDA MIndirectX, 0x10]

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
-- D64 generation (4 props)
-- ---------------------------------------------------------------------------

prop_d64ImageSize :: [Word8] -> Property
prop_d64ImageSize bs =
    length bs < 5000 ==>
        let prg = toPRG 0x0801 bs
        in  length (toD64 "TEST" prg) == 174848

prop_d64BamHeader :: Bool
prop_d64BamHeader =
    let img = toD64 "TEST" [0x01, 0x08]
        -- BAM is at track 18 sector 0.  Track 18 starts after tracks 1-17.
        -- Tracks 1-17: 17 * 21 = 357 sectors.  Offset = 357 * 256 = 91392.
        bamOffset = 91392
    in  img !! bamOffset == 18           -- directory pointer: track 18
     && img !! (bamOffset + 1) == 1      -- directory pointer: sector 1
     && img !! (bamOffset + 2) == 0x41   -- DOS version 'A'

prop_d64DirectoryFileType :: Bool
prop_d64DirectoryFileType =
    let img = toD64 "TEST" [0x01, 0x08, 0xEA]
        -- Directory is at track 18 sector 1.  Offset = (357 + 1) * 256 = 91648.
        dirOffset = 91648
    in  img !! (dirOffset + 2) == 0x82   -- closed PRG

prop_d64RoundTrip :: [Word8] -> Property
prop_d64RoundTrip payload =
    length payload > 0 && length payload < 2000 ==>
        let prg = toPRG 0x0801 payload
            img = toD64 "TEST" prg
            -- Follow T/S chain starting from directory entry
            dirOffset = 91648
            firstTrack  = fromIntegral (img !! (dirOffset + 3)) :: Int
            firstSector = fromIntegral (img !! (dirOffset + 4)) :: Int
            extracted = followChain img (firstTrack, firstSector)
        in  extracted == prg
  where
    -- Compute absolute byte offset for a (track, sector) pair.
    -- Tracks 1-17: 21 sectors each, 18-24: 19, 25-30: 18, 31-35: 17.
    tsOffset :: (Int, Int) -> Int
    tsOffset (t, s) =
        let spt = [0] ++ replicate 17 21 ++ replicate 7 19
                       ++ replicate 6 18 ++ replicate 5 17
        in  (sum (take t spt) + s) * 256

    followChain :: [Word8] -> (Int, Int) -> [Word8]
    followChain img (t, s)
        | t == 0    = []  -- should not happen for first call
        | otherwise =
            let off = tsOffset (t, s)
                nextT = fromIntegral (img !! off) :: Int
                nextS = fromIntegral (img !! (off + 1)) :: Int
            in  if nextT == 0
                then take (nextS - 1) (drop (off + 2) img)  -- last sector
                else take 254 (drop (off + 2) img) ++ followChain img (nextT, nextS)

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    ok <- fmap and . sequence $
        [ section "lo/hi byte helpers"
        , check "lo masks low byte"       prop_loMasks
        , check "hi masks high byte"      prop_hiMasks
        , check "lo/hi roundtrip"         prop_lohiRoundtrip

        , section "Opcode table integrity"
        , checkOnce "table has 151 entries"      prop_opcodeTableSize
        , checkOnce "table values are injective" prop_opcodeTableInjective
        , checkOnce "all 56 opcodes present"     prop_opcodeTableAllOpcodes

        , section "Instruction size determinism"
        , check "mode determines size"      prop_modeSizeCorrect
        , check "same mode → same size"     prop_sizeDeterministic

        , section "Instruction byte correctness"
        , check "first byte is opcode"        prop_firstByteIsOpcode
        , check "operand bytes preserved"     prop_operandBytesPreserved

        , section "EDSL instruction functions"
        , check "lda (Imm v)"   prop_ldaImm
        , check "lda (ZP v)"    prop_ldaZP
        , check "lda (Abs v)"   prop_ldaAbs
        , check "sta (ZPX v)"   prop_staZPX
        , check "ldx (AbsY v)"  prop_ldxAbsY
        , check "lda (IndX v)"  prop_ldaIndX
        , check "lda (IndY v)"  prop_ldaIndY
        , checkOnce "nop (implied)"      prop_nopImplied
        , checkOnce "asl_a (accumulator)" prop_aslAccumulator

        , section "Monad / PC tracking"
        , check "label returns origin"     prop_labelReturnsOrigin
        , check "emit advances PC"         prop_emitAdvancesPC
        , check "sequence additive"        prop_sequenceAdditive
        , check "sequence bytes concat"    prop_sequenceBytesConcat

        , section "Random programs"
        , check "program length"         prop_programLength
        , check "assembly deterministic" prop_assemblyDeterministic
        , check "program concat"         prop_programConcat

        , section "Branch offsets"
        , checkOnce "backward branch"          prop_backwardBranch
        , checkOnce "forward branch (mdo)"     prop_forwardBranchMdo
        , checkOnce "backward offset byte"     prop_branchOffsetBackward
        , checkOnce "forward offset byte"      prop_branchOffsetForward

        , section "Zero-page allocation"
        , checkOnce "single-byte allocation"     prop_allocZPSingleByte
        , checkOnce "multi-byte contiguous"      prop_allocZPMultiByte
        , checkOnce "allocations don't overlap"  prop_allocZPNoOverlap
        , checkOnce "allocZP works inside mdo"   prop_allocZPInMdo

        , section "Addressing mode sugar"
        , check "# immediate"           prop_sugarImmediate
        , check "bare Word8 → ZP"       prop_sugarBareWord8
        , check "bare Word16 → Abs"     prop_sugarBareWord16
        , checkOnce "asl A == asl_a"    prop_sugarAccumulatorAsl
        , checkOnce "lsr A == lsr_a"    prop_sugarAccumulatorLsr
        , checkOnce "rol A == rol_a"    prop_sugarAccumulatorRol
        , checkOnce "ror A == ror_a"    prop_sugarAccumulatorRor
        , check "(Word8, X) → ZPX"     prop_sugarZPX
        , check "(Word8, Y) → ZPY"     prop_sugarZPY
        , check "(Word16, X) → AbsX"   prop_sugarAbsX
        , check "(Word16, Y) → AbsY"   prop_sugarAbsY
        , check "Word8 ! Y → IndY"     prop_sugarIndY
        , check "Word8 ! X → IndX"     prop_sugarIndX
        , checkOnce "Var8 → ZP"         prop_sugarVar8
        , checkOnce "Ptr ! Y → IndY"    prop_sugarPtrIndY
        , checkOnce "Ptr arithmetic"    prop_sugarPtrArith
        , checkOnce "Var16 lo16/hi16"   prop_sugarVar16Helpers
        , checkOnce "typed allocators"  prop_sugarAllocators
        , checkOnce "Var8 tuple (v,X)"  prop_sugarVar8X
        , checkOnce "Ptr ! X → IndX"    prop_sugarPtrIndX

        , section "PRG generation"
        , check "prg length = input + 2"   prop_prgLength
        , check "prg header matches lo/hi" prop_prgHeader
        , check "prg payload preserved"    prop_prgPayload

        , section "D64 generation"
        , check "d64 image size = 174848"  prop_d64ImageSize
        , checkOnce "d64 BAM header bytes"     prop_d64BamHeader
        , checkOnce "d64 directory file type"  prop_d64DirectoryFileType
        , check "d64 round-trip T/S chain" prop_d64RoundTrip
        ]
    if ok then putStrLn "\nAll properties passed."
          else putStrLn "\nSome properties FAILED." >> exitFailure

section :: String -> IO Bool
section name = do
    putStrLn $ "\n=== " ++ name ++ " ==="
    pure True

-- | Deterministic args: fixed seed, 1000 test cases.
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
