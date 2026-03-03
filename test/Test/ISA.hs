module Test.ISA (tests) where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.List (nub)
import Data.Word (Word8, Word16)
import Test.QuickCheck hiding ((.&.))

import Asm.Monad (MonadASM(..), lo, hi)
import ISA.Mos6502 (Opcode(..), Instruction(..), encode, decode, instrSize, baseCycles)
import Test.Helpers

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
-- Opcode table integrity (3 props) — via decode table
-- ---------------------------------------------------------------------------

prop_opcodeTableSize :: Bool
prop_opcodeTableSize =
    let count = length [ () | i <- [0x00..0xFF :: Word8]
                            , Just _ <- [decode i] ]
    in  count == 151

prop_opcodeTableInjective :: Bool
prop_opcodeTableInjective =
    let instrs = [ f 0 0 | i <- [0x00..0xFF :: Word8]
                          , Just (_, f) <- [decode i] ]
    in  length (nub instrs) == length instrs

prop_opcodeTableAllOpcodes :: Bool
prop_opcodeTableAllOpcodes =
    let opcodes = nub [ opc | i <- [0x00..0xFF :: Word8]
                            , Just (_, f) <- [decode i]
                            , let Instruction opc _ = f 0 0 ]
    in  length opcodes == length [minBound .. maxBound :: Opcode]

-- ---------------------------------------------------------------------------
-- Instruction size (2 props) — critical for MonadFix
-- ---------------------------------------------------------------------------

prop_instrSizeCorrect :: TestInsn -> Bool
prop_instrSizeCorrect (TestInsn instr) =
    length (asm (emitBytes (encode instr))) == instrSize instr

prop_instrSizeMatchesEncode :: TestInsn -> Bool
prop_instrSizeMatchesEncode (TestInsn instr) =
    instrSize instr == length (encode instr)

-- ---------------------------------------------------------------------------
-- Instruction byte correctness (2 props)
-- ---------------------------------------------------------------------------

prop_firstByteDecodes :: TestInsn -> Bool
prop_firstByteDecodes (TestInsn instr@(Instruction opc _)) =
    case encode instr of
        (b:_) -> case decode b of
            Just (_, f) -> let Instruction opc' _ = f 0 0 in opc == opc'
            Nothing -> False
        [] -> False

prop_encodeDecodeRoundtrip :: TestInsn -> Bool
prop_encodeDecodeRoundtrip (TestInsn instr) =
    case encode instr of
        [] -> False
        (opByte : operands) ->
            let b1 = case operands of (x:_) -> x; [] -> 0
                b2 = case operands of (_:x:_) -> x; _ -> 0
            in case decode opByte of
                Nothing     -> False
                Just (n, f) -> n == length operands && f b1 b2 == instr

-- ---------------------------------------------------------------------------
-- Random programs (3 props)
-- ---------------------------------------------------------------------------

prop_programLength :: [TestInsn] -> Property
prop_programLength insns =
    length insns < 100 ==>
        let bytes = asm (mapM_ (emitBytes . testInsnBytes) insns)
        in  length bytes == sum (map (\(TestInsn i) -> instrSize i) insns)

prop_assemblyDeterministic :: [TestInsn] -> Property
prop_assemblyDeterministic insns =
    length insns < 100 ==>
        let prog = mapM_ (emitBytes . testInsnBytes) insns
        in  asm prog == asm prog

prop_programConcat :: [TestInsn] -> Property
prop_programConcat insns =
    length insns < 100 ==>
        asm (mapM_ (emitBytes . testInsnBytes) insns) == concatMap testInsnBytes insns

-- ---------------------------------------------------------------------------
-- baseCycles / decode coverage (3 props)
-- ---------------------------------------------------------------------------

prop_baseCyclesPositive :: Bool
prop_baseCyclesPositive =
    all (\(TestInsn instr) -> baseCycles instr > 0) allValidInsns
  where
    allValidInsns = [ TestInsn (f 0 0) | i <- [0x00..0xFF :: Word8]
                                       , Just (_, f) <- [decode i] ]

prop_decodeCoverage :: Bool
prop_decodeCoverage =
    all roundtrips [0x00..0xFF :: Word8]
  where
    roundtrips opByte = case decode opByte of
        Nothing     -> True  -- illegal opcode, skip
        Just (n, f) ->
            let instr = f 0x42 0x77
                encoded = encode instr
            in  case encoded of
                    (b:_) -> b == opByte && length encoded == n + 1
                    []    -> False

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "lo/hi byte helpers"
    , check "lo masks low byte"       prop_loMasks
    , check "hi masks high byte"      prop_hiMasks
    , check "lo/hi roundtrip"         prop_lohiRoundtrip

    , section "Opcode table integrity"
    , checkOnce "table has 151 entries"      prop_opcodeTableSize
    , checkOnce "table values are injective" prop_opcodeTableInjective
    , checkOnce "all 56 opcodes present"     prop_opcodeTableAllOpcodes

    , section "Instruction size determinism"
    , check "instrSize matches emit"      prop_instrSizeCorrect
    , check "instrSize matches encode"    prop_instrSizeMatchesEncode

    , section "Instruction byte correctness"
    , check "first byte decodes to same opcode" prop_firstByteDecodes
    , check "encode/decode roundtrip"           prop_encodeDecodeRoundtrip

    , section "Random programs"
    , check "program length"         prop_programLength
    , check "assembly deterministic" prop_assemblyDeterministic
    , check "program concat"         prop_programConcat

    , section "Cycle counts and decode coverage"
    , checkOnce "baseCycles > 0 for all"   prop_baseCyclesPositive
    , checkOnce "all 151 opcodes roundtrip" prop_decodeCoverage
    ]
