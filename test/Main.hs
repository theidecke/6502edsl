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

import Data.Char (ord)

import Control.Exception (evaluate, try, SomeException)

import Asm.Monad (ASM, TargetConfig(..), assemble, assembleWithLabels, emit, label, allocZP, lo, hi)
import Asm.Mos6502
import Asm.Mos6502.Memory (align, alignPage, samePage)
import Asm.Mos6502.Control (if_, if_eq, if_ne, if_cs, if_cc, if_pl, if_mi, while_, for_x, for_y, loop_)
import Asm.Mos6502.Debug (fitsIn, annotate)
import Asm.Mos6502.Ops16 (add16, sub16, inc16, dec16, cmp16, mov16, load16)
import ISA.Mos6502 (Opcode(..))
import Target.C64.Data (byte, word, petscii, pstring, charToPetscii)
import Target.C64.Debug (exportViceLabels)
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
-- Branch range validation (4 props)
-- ---------------------------------------------------------------------------

prop_branchMaxForward :: Bool
prop_branchMaxForward =
    -- BEQ skip; 127 bytes of NOPs; skip:  →  offset byte is 0x7F (+127)
    let bytes = asm $ mdo
            beq skip
            emit (replicate 127 0xEA)
            skip <- label
            pure ()
    in  bytes !! 1 == 0x7F

prop_branchMaxBackward :: Bool
prop_branchMaxBackward =
    -- 126 NOPs; loop: NOP; NOP; BNE loop  →  offset is -128 = 0x80
    -- PC at BNE = 128, target = 126, diff = 126 - 128 - 2 = -4… let's compute:
    -- We need: target - pc - 2 = -128, so pc - target = 126, meaning 126 bytes
    -- between target and the branch + its operand.
    -- loop: (1 NOP = 1 byte) then 125 NOPs, then BNE = offset -128
    let bytes = asm $ do
            loop <- label
            emit (replicate 126 0xEA)
            bne loop
    in  let offset = bytes !! (126 + 1)  -- byte after BNE opcode
        in  offset == 0x80  -- -128 as unsigned

prop_branchOutOfRangeForward :: IO Bool
prop_branchOutOfRangeForward = do
    -- BEQ skip; 128 NOPs; skip:  →  offset +128, out of range
    let bytes = asm $ mdo
            beq skip
            emit (replicate 128 0xEA)
            skip <- label
            pure ()
    -- Force all byte values (not just the spine) to trigger the lazy error
    result <- try (evaluate (sum bytes)) :: IO (Either SomeException Word8)
    pure $ isLeft result

prop_branchOutOfRangeBackward :: IO Bool
prop_branchOutOfRangeBackward = do
    -- loop: 127 NOPs; BNE loop  →  offset -129, out of range
    let bytes = asm $ do
            loop <- label
            emit (replicate 127 0xEA)
            bne loop
    result <- try (evaluate (sum bytes)) :: IO (Either SomeException Word8)
    pure $ isLeft result

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
-- Asm.Mos6502.Memory (6 props)
-- ---------------------------------------------------------------------------

prop_alignAlreadyAligned :: Bool
prop_alignAlreadyAligned = asmAt 0x0100 (align 256) == []

prop_alignPadding :: Bool
prop_alignPadding = asmAt 0x0001 (align 4) == [0x00, 0x00, 0x00]

prop_alignOne :: Bool
prop_alignOne = asmAt 0x0003 (align 1) == []

prop_alignZero :: Bool
prop_alignZero = asmAt 0x0003 (align 0) == []

prop_alignAfterCode :: Bool
prop_alignAfterCode =
    -- nop at 0xF0 → PC=0xF1, 0xF1 mod 8 = 1, padding = 7
    asmAt 0x00F0 (nop >> align 8) == [0xEA] ++ replicate 7 0x00

prop_alignPage :: Bool
prop_alignPage =
    -- nop at 0xF0 → PC=0xF1, 0xF1 mod 256 = 241, padding = 15
    let bytes = asmAt 0x00F0 (nop >> alignPage)
    in  bytes == 0xEA : replicate 15 0x00

-- ---------------------------------------------------------------------------
-- Target.C64.Data (12 props)
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
-- Asm.Mos6502.Control (12 props)
-- ---------------------------------------------------------------------------

-- Helper: emit if_ variant with nop/nop then/else blocks
ifBytes :: (ASM () -> ASM () -> ASM ()) -> [Word8]
ifBytes f = asm (f nop nop)

-- if_eq uses BNE ($D0) to skip then-block
prop_ifEqUsesBne :: Bool
prop_ifEqUsesBne = ifBytes if_eq == [0xD0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- if_ne uses BEQ ($F0)
prop_ifNeUsesBeq :: Bool
prop_ifNeUsesBeq = ifBytes if_ne == [0xF0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- if_cs uses BCC ($90)
prop_ifCsUsesBcc :: Bool
prop_ifCsUsesBcc = ifBytes if_cs == [0x90, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- if_cc uses BCS ($B0)
prop_ifCcUsesBcs :: Bool
prop_ifCcUsesBcs = ifBytes if_cc == [0xB0, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- if_pl uses BMI ($30)
prop_ifPlUsesBmi :: Bool
prop_ifPlUsesBmi = ifBytes if_pl == [0x30, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- if_mi uses BPL ($10)
prop_ifMiUsesBpl :: Bool
prop_ifMiUsesBpl = ifBytes if_mi == [0x10, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- Larger then-block shifts offsets correctly
prop_ifLargerThenBlock :: Bool
prop_ifLargerThenBlock =
    -- then = 3 nops → BNE offset=6, JMP to $0009
    asm (if_eq (nop >> nop >> nop) nop)
    == [0xD0, 0x06, 0xEA, 0xEA, 0xEA, 0x4C, 0x09, 0x00, 0xEA]

-- Generalized if_ with custom branch
prop_ifGeneralized :: Bool
prop_ifGeneralized =
    -- if_ bvc = "if overflow clear, then; else"
    asm (if_ bvc nop nop) == [0x50, 0x04, 0xEA, 0x4C, 0x07, 0x00, 0xEA]

-- for_x: LDX #count, body, DEX, BNE back
prop_forXBytes :: Bool
prop_forXBytes = asm (for_x 3 nop) == [0xA2, 0x03, 0xEA, 0xCA, 0xD0, 0xFC]

-- for_y: LDY #count, body, DEY, BNE back
prop_forYBytes :: Bool
prop_forYBytes = asm (for_y 5 nop) == [0xA0, 0x05, 0xEA, 0x88, 0xD0, 0xFC]

-- for_x with larger body adjusts branch offset
prop_forXMultiByteBody :: Bool
prop_forXMultiByteBody =
    -- body = 3 nops → BNE offset = 2-(3+3+1+2) = -6 = 0xFA
    asm (for_x 2 (nop >> nop >> nop)) == [0xA2, 0x02, 0xEA, 0xEA, 0xEA, 0xCA, 0xD0, 0xFA]

-- loop_: body, JMP back
prop_loopBytes :: Bool
prop_loopBytes = asm (loop_ nop) == [0xEA, 0x4C, 0x00, 0x00]

-- while_: cond, exitBranch, body, JMP back
prop_whileBytes :: Bool
prop_whileBytes =
    -- while_ beq (cmp #5) nop → loop while Z=0
    -- top@0: C9 05, beq exit@2: F0 04, nop@4: EA, jmp top@5: 4C 00 00, exit@8
    asm (while_ beq (cmp # 0x05) nop)
    == [0xC9, 0x05, 0xF0, 0x04, 0xEA, 0x4C, 0x00, 0x00]

-- ---------------------------------------------------------------------------
-- Asm.Mos6502.Ops16 (7 props)
-- ---------------------------------------------------------------------------

-- Helper: assemble with ZP vars available (0x02..0x0F)
asmZP :: ASM a -> [Word8]
asmZP = snd . assemble (zpConfig [0x02..0x0F])

-- load16 v imm → LDA #lo, STA lo16, LDA #hi, STA hi16
prop_load16Bytes :: Bool
prop_load16Bytes =
    asmZP (allocVar16 >>= \v -> load16 v 0x1234)
    == [0xA9, 0x34, 0x85, 0x02, 0xA9, 0x12, 0x85, 0x03]

-- inc16 v → INC lo, BNE +2, INC hi
prop_inc16Bytes :: Bool
prop_inc16Bytes =
    asmZP (allocVar16 >>= inc16)
    == [0xE6, 0x02, 0xD0, 0x02, 0xE6, 0x03]

-- dec16 v → LDA lo, BNE +2, DEC hi, DEC lo
prop_dec16Bytes :: Bool
prop_dec16Bytes =
    asmZP (allocVar16 >>= dec16)
    == [0xA5, 0x02, 0xD0, 0x02, 0xC6, 0x03, 0xC6, 0x02]

-- add16 dst a b → CLC, LDA lo(a), ADC lo(b), STA lo(dst), ...
prop_add16Bytes :: Bool
prop_add16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16  -- 0x02,0x03
            a   <- allocVar16  -- 0x04,0x05
            b   <- allocVar16  -- 0x06,0x07
            add16 dst a b
    in  bytes == [ 0x18                                -- CLC
                 , 0xA5, 0x04, 0x65, 0x06, 0x85, 0x02 -- lo: LDA, ADC, STA
                 , 0xA5, 0x05, 0x65, 0x07, 0x85, 0x03 -- hi: LDA, ADC, STA
                 ]

-- sub16 dst a b → SEC, LDA lo(a), SBC lo(b), STA lo(dst), ...
prop_sub16Bytes :: Bool
prop_sub16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16  -- 0x02,0x03
            a   <- allocVar16  -- 0x04,0x05
            b   <- allocVar16  -- 0x06,0x07
            sub16 dst a b
    in  bytes == [ 0x38                                -- SEC
                 , 0xA5, 0x04, 0xE5, 0x06, 0x85, 0x02 -- lo: LDA, SBC, STA
                 , 0xA5, 0x05, 0xE5, 0x07, 0x85, 0x03 -- hi: LDA, SBC, STA
                 ]

-- mov16 dst src → LDA lo(src), STA lo(dst), LDA hi(src), STA hi(dst)
prop_mov16Bytes :: Bool
prop_mov16Bytes =
    let bytes = asmZP $ do
            dst <- allocVar16  -- 0x02,0x03
            src <- allocVar16  -- 0x04,0x05
            mov16 dst src
    in  bytes == [0xA5, 0x04, 0x85, 0x02, 0xA5, 0x05, 0x85, 0x03]

-- cmp16 a b → LDA lo(a), CMP lo(b), LDA hi(a), SBC hi(b)
prop_cmp16Bytes :: Bool
prop_cmp16Bytes =
    let bytes = asmZP $ do
            a <- allocVar16  -- 0x02,0x03
            b <- allocVar16  -- 0x04,0x05
            cmp16 a b
    in  bytes == [0xA5, 0x02, 0xC5, 0x04, 0xA5, 0x03, 0xE5, 0x05]

-- ---------------------------------------------------------------------------
-- VICE label export (3 props)
-- ---------------------------------------------------------------------------

prop_viceLabelsFormat :: Bool
prop_viceLabelsFormat =
    exportViceLabels [("main", 0x0810), ("loop", 0x0820)]
    == "al C:0810 .main\nal C:0820 .loop\n"

prop_viceLabelsEmpty :: Bool
prop_viceLabelsEmpty = exportViceLabels [] == ""

prop_viceLabelsLeadingZeros :: Bool
prop_viceLabelsLeadingZeros =
    exportViceLabels [("start", 0x0010)] == "al C:0010 .start\n"

-- ---------------------------------------------------------------------------
-- samePage (2 props)
-- ---------------------------------------------------------------------------

prop_samePagePasses :: Bool
prop_samePagePasses =
    -- origin $0800, samePage $08FF → no error, no bytes
    snd (assemble (simpleConfig 0x0800) (samePage 0x08FF)) == []

prop_samePageFails :: IO Bool
prop_samePageFails = do
    -- origin $08FF, emit 2 bytes → PC=$0901, samePage $0800 → error
    let prog = do emit [0x00, 0x00]; samePage 0x0800
        bytes = snd (assemble (simpleConfig 0x08FF) prog)
    result <- try (evaluate (length bytes)) :: IO (Either SomeException Int)
    pure $ isLeft result

-- ---------------------------------------------------------------------------
-- fitsIn (4 props)
-- ---------------------------------------------------------------------------

prop_fitsInPasses :: Bool
prop_fitsInPasses =
    snd (assemble (simpleConfig 0x0000) (fitsIn 5 (nop >> nop))) == [0xEA, 0xEA]

prop_fitsInFails :: IO Bool
prop_fitsInFails = do
    let prog = fitsIn 1 (nop >> nop)
        bytes = snd (assemble (simpleConfig 0x0000) prog)
    result <- try (evaluate (length bytes)) :: IO (Either SomeException Int)
    pure $ isLeft result

prop_fitsInExact :: Bool
prop_fitsInExact =
    snd (assemble (simpleConfig 0x0000) (fitsIn 1 nop)) == [0xEA]

prop_fitsInPreservesResult :: Bool
prop_fitsInPreservesResult =
    let cfg = simpleConfig 0x0800
        (pc, _) = assemble cfg (fitsIn 10 (lda # 0x42 >> label))
    in  pc == 0x0802

-- ---------------------------------------------------------------------------
-- annotate (3 props)
-- ---------------------------------------------------------------------------

prop_annotateRecordsLabel :: Bool
prop_annotateRecordsLabel =
    let cfg = simpleConfig 0x0800
        (_, _, labels) = assembleWithLabels cfg (annotate "main" nop)
    in  labels == [("main", 0x0800)]

prop_annotateMultiple :: Bool
prop_annotateMultiple =
    let cfg = simpleConfig 0x0800
        (_, _, labels) = assembleWithLabels cfg $ do
            annotate "first" nop
            annotate "second" nop
    in  labels == [("first", 0x0800), ("second", 0x0801)]

prop_annotatePreservesResult :: Bool
prop_annotatePreservesResult =
    let cfg = zpConfig [0x10..0x19]
        (v, _, _) = assembleWithLabels cfg (annotate "x" allocVar8)
    in  v == Var8 0x10

-- ---------------------------------------------------------------------------
-- assembleWithLabels (2 props)
-- ---------------------------------------------------------------------------

prop_assembleWithLabelsSameBytes :: Bool
prop_assembleWithLabelsSameBytes =
    let cfg = simpleConfig 0x0800
        prog = nop >> nop >> rts
        (_, bytesA) = assemble cfg prog
        (_, bytesB, _) = assembleWithLabels cfg prog
    in  bytesA == bytesB

prop_assembleWithLabelsOrder :: Bool
prop_assembleWithLabelsOrder =
    let cfg = simpleConfig 0x0800
        (_, _, labels) = assembleWithLabels cfg $ do
            annotate "a" nop
            annotate "b" nop
            annotate "c" nop
    in  map fst labels == ["a", "b", "c"]

-- ---------------------------------------------------------------------------
-- Helpers for testing error cases
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

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

        , section "Branch range validation"
        , checkOnce "max forward (+127)"         prop_branchMaxForward
        , checkOnce "max backward (-128)"        prop_branchMaxBackward
        , checkIO   "out of range forward"       prop_branchOutOfRangeForward
        , checkIO   "out of range backward"      prop_branchOutOfRangeBackward

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

        , section "Memory alignment"
        , checkOnce "align already aligned"     prop_alignAlreadyAligned
        , checkOnce "align adds padding"        prop_alignPadding
        , checkOnce "align 1 is no-op"          prop_alignOne
        , checkOnce "align 0 is no-op"          prop_alignZero
        , checkOnce "align after code"          prop_alignAfterCode
        , checkOnce "alignPage to 256"          prop_alignPage

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

        , section "Control flow"
        , checkOnce "if_eq uses BNE"            prop_ifEqUsesBne
        , checkOnce "if_ne uses BEQ"            prop_ifNeUsesBeq
        , checkOnce "if_cs uses BCC"            prop_ifCsUsesBcc
        , checkOnce "if_cc uses BCS"            prop_ifCcUsesBcs
        , checkOnce "if_pl uses BMI"            prop_ifPlUsesBmi
        , checkOnce "if_mi uses BPL"            prop_ifMiUsesBpl
        , checkOnce "if_ larger then block"     prop_ifLargerThenBlock
        , checkOnce "if_ generalized (bvc)"     prop_ifGeneralized
        , checkOnce "for_x bytes"               prop_forXBytes
        , checkOnce "for_y bytes"               prop_forYBytes
        , checkOnce "for_x multi-byte body"     prop_forXMultiByteBody
        , checkOnce "loop_ bytes"               prop_loopBytes
        , checkOnce "while_ bytes"              prop_whileBytes

        , section "16-bit operations"
        , checkOnce "load16 bytes"              prop_load16Bytes
        , checkOnce "inc16 bytes"               prop_inc16Bytes
        , checkOnce "dec16 bytes"               prop_dec16Bytes
        , checkOnce "add16 bytes"               prop_add16Bytes
        , checkOnce "sub16 bytes"               prop_sub16Bytes
        , checkOnce "mov16 bytes"               prop_mov16Bytes
        , checkOnce "cmp16 bytes"               prop_cmp16Bytes

        , section "VICE label export"
        , checkOnce "format check"              prop_viceLabelsFormat
        , checkOnce "empty list"                prop_viceLabelsEmpty
        , checkOnce "leading zeros"             prop_viceLabelsLeadingZeros

        , section "samePage assertion"
        , checkOnce "same page passes"          prop_samePagePasses
        , checkIO   "different page fails"      prop_samePageFails

        , section "fitsIn assertion"
        , checkOnce "within limit passes"       prop_fitsInPasses
        , checkIO   "exceeds limit fails"       prop_fitsInFails
        , checkOnce "exact limit passes"        prop_fitsInExact
        , checkOnce "return value preserved"    prop_fitsInPreservesResult

        , section "annotate"
        , checkOnce "records label"             prop_annotateRecordsLabel
        , checkOnce "multiple annotations"      prop_annotateMultiple
        , checkOnce "preserves result"          prop_annotatePreservesResult

        , section "assembleWithLabels"
        , checkOnce "same bytes as assemble"    prop_assembleWithLabelsSameBytes
        , checkOnce "labels in source order"    prop_assembleWithLabelsOrder
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

checkIO :: String -> IO Bool -> IO Bool
checkIO name action = do
    putStr $ "  " ++ name ++ ": "
    ok <- action
    putStrLn $ if ok then "+++ OK" else "*** FAILED"
    pure ok
