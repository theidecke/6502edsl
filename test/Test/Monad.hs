{-# LANGUAGE RecursiveDo #-}

module Test.Monad (tests) where

import Control.Exception (evaluate, try, SomeException)
import Data.Word (Word8, Word16)
import Test.QuickCheck hiding (label)

import Asm.Monad (assemble, emit, label, allocZP)
import Asm.Mos6502
import Test.Helpers

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
-- Branch offsets (4 props)
-- ---------------------------------------------------------------------------

prop_backwardBranch :: Bool
prop_backwardBranch =
    let bytes = asm $ do
            loop <- label
            dex
            bne loop
    in  bytes == [0xCA, 0xD0, 0xFD]

prop_forwardBranchMdo :: Bool
prop_forwardBranchMdo =
    let bytes = asm $ mdo
            lda (ZP 0x10)
            beq skip
            lda (Imm 0xFF)
            skip <- label
            rts
    in  bytes == [0xA5, 0x10, 0xF0, 0x02, 0xA9, 0xFF, 0x60]

prop_branchOffsetBackward :: Bool
prop_branchOffsetBackward =
    let bytes = asm $ do
            loop <- label
            nop
            bne loop
    in  bytes !! 2 == 0xFD

prop_branchOffsetForward :: Bool
prop_branchOffsetForward =
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
    let bytes = asm $ mdo
            beq skip
            emit (replicate 127 0xEA)
            skip <- label
            pure ()
    in  bytes !! 1 == 0x7F

prop_branchMaxBackward :: Bool
prop_branchMaxBackward =
    let bytes = asm $ do
            loop <- label
            emit (replicate 126 0xEA)
            bne loop
    in  let offset = bytes !! (126 + 1)
        in  offset == 0x80

prop_branchOutOfRangeForward :: IO Bool
prop_branchOutOfRangeForward = do
    let bytes = asm $ mdo
            beq skip
            emit (replicate 128 0xEA)
            skip <- label
            pure ()
    result <- try (evaluate (sum bytes)) :: IO (Either SomeException Word8)
    pure $ isLeft result

prop_branchOutOfRangeBackward :: IO Bool
prop_branchOutOfRangeBackward = do
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
    in  addr == 0x02

prop_allocZPMultiByte :: Bool
prop_allocZPMultiByte =
    let cfg = zpConfig ([0x02] ++ [0xFB .. 0xFE])
        (addr, _) = assemble cfg (allocZP 3)
    in  addr == 0xFB

prop_allocZPNoOverlap :: Bool
prop_allocZPNoOverlap =
    let cfg = zpConfig [0x10 .. 0x19]
        ((a1, a2), _) = assemble cfg $ do
            x <- allocZP 4
            y <- allocZP 4
            pure (x, y)
    in  a1 == 0x10 && a2 == 0x14

prop_allocZPInMdo :: Bool
prop_allocZPInMdo =
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
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "Monad / PC tracking"
    , check "label returns origin"     prop_labelReturnsOrigin
    , check "emit advances PC"         prop_emitAdvancesPC
    , check "sequence additive"        prop_sequenceAdditive
    , check "sequence bytes concat"    prop_sequenceBytesConcat

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
    ]
