module Test.Memory (tests) where

import Control.Exception (evaluate, try, SomeException)
import Data.Map.Strict qualified as Map

import Data.Word (Word16)

import Asm.Monad (assemble, assembleWithLabels, emit, currentPC)
import Asm.Mos6502 (Var8(..), allocVar8, nop, lda, rts, (#))
import Asm.Mos6502.Memory (align, alignPage, samePage)
import Asm.Mos6502.Debug (fitsIn, annotate)
import Test.Helpers

-- ---------------------------------------------------------------------------
-- Memory alignment (6 props)
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
    asmAt 0x00F0 (nop >> align 8) == [0xEA] ++ replicate 7 0x00

prop_alignPage :: Bool
prop_alignPage =
    let bytes = asmAt 0x00F0 (nop >> alignPage)
    in  bytes == 0xEA : replicate 15 0x00

-- ---------------------------------------------------------------------------
-- samePage (2 props)
-- ---------------------------------------------------------------------------

prop_samePagePasses :: Bool
prop_samePagePasses =
    snd (assemble (simpleConfig 0x0800) (samePage (0x08FF :: Word16))) == []

prop_samePageFails :: IO Bool
prop_samePageFails = do
    let prog = do emit [0x00, 0x00]; samePage (0x0800 :: Word16)
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
        (pc, _) = assemble cfg (fitsIn 10 (lda # 0x42 >> currentPC))
    in  pc == 0x0802

-- ---------------------------------------------------------------------------
-- annotate (4 props)
-- ---------------------------------------------------------------------------

prop_annotateRecordsLabel :: Bool
prop_annotateRecordsLabel =
    let cfg = simpleConfig 0x0800
        (_, _, annotations, _) = assembleWithLabels cfg (annotate "main" nop)
    in  annotations == Map.fromList [(0x0800, ["main"])]

prop_annotateMultiple :: Bool
prop_annotateMultiple =
    let cfg = simpleConfig 0x0800
        (_, _, annotations, _) = assembleWithLabels cfg $ do
            annotate "first" nop
            annotate "second" nop
    in  annotations == Map.fromList [(0x0800, ["first"]), (0x0801, ["second"])]

prop_annotateNested :: Bool
prop_annotateNested =
    let cfg = simpleConfig 0x0800
        (_, _, annotations, _) = assembleWithLabels cfg $
            annotate "outer" (nop >> annotate "inner" nop)
    in  annotations == Map.fromList [(0x0800, ["outer"]), (0x0801, ["inner", "outer"])]

prop_annotatePreservesResult :: Bool
prop_annotatePreservesResult =
    let cfg = zpConfig [0x10..0x19]
        (v, _, _, _) = assembleWithLabels cfg (annotate "x" allocVar8)
    in  v == Var8 0x10

-- ---------------------------------------------------------------------------
-- assembleWithLabels (2 props)
-- ---------------------------------------------------------------------------

prop_assembleWithLabelsSameBytes :: Bool
prop_assembleWithLabelsSameBytes =
    let cfg = simpleConfig 0x0800
        prog = nop >> nop >> rts
        (_, bytesA) = assemble cfg prog
        (_, bytesB, _, _) = assembleWithLabels cfg prog
    in  bytesA == bytesB

prop_assembleWithLabelsOrder :: Bool
prop_assembleWithLabelsOrder =
    let cfg = simpleConfig 0x0800
        (_, _, annotations, _) = assembleWithLabels cfg $ do
            annotate "a" nop
            annotate "b" nop
            annotate "c" nop
    in  Map.elems annotations == [["a"], ["b"], ["c"]]

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "Memory alignment"
    , checkOnce "align already aligned"     prop_alignAlreadyAligned
    , checkOnce "align adds padding"        prop_alignPadding
    , checkOnce "align 1 is no-op"          prop_alignOne
    , checkOnce "align 0 is no-op"          prop_alignZero
    , checkOnce "align after code"          prop_alignAfterCode
    , checkOnce "alignPage to 256"          prop_alignPage

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
    , checkOnce "nested annotations"        prop_annotateNested
    , checkOnce "preserves result"          prop_annotatePreservesResult

    , section "assembleWithLabels"
    , checkOnce "same bytes as assemble"    prop_assembleWithLabelsSameBytes
    , checkOnce "labels in source order"    prop_assembleWithLabelsOrder
    ]
