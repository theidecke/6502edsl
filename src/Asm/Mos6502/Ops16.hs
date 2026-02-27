{-# LANGUAGE RecursiveDo #-}

module Asm.Mos6502.Ops16
    ( add16, sub16, inc16, dec16
    , cmp16, mov16, load16
    ) where

import Data.Word (Word16)

import Asm.Monad (ASM, label, lo, hi)
import Asm.Mos6502 (Var16, lo16, hi16, lda, sta, adc, sbc, inc, dec, cmp, clc, sec, bne, (#))

-- | 16-bit addition: @dst = a + b@.
add16 :: Var16 -> Var16 -> Var16 -> ASM ()
add16 dst a b = do
    clc
    lda (lo16 a); adc (lo16 b); sta (lo16 dst)
    lda (hi16 a); adc (hi16 b); sta (hi16 dst)

-- | 16-bit subtraction: @dst = a - b@.
sub16 :: Var16 -> Var16 -> Var16 -> ASM ()
sub16 dst a b = do
    sec
    lda (lo16 a); sbc (lo16 b); sta (lo16 dst)
    lda (hi16 a); sbc (hi16 b); sta (hi16 dst)

-- | 16-bit increment: @v++@.
inc16 :: Var16 -> ASM ()
inc16 v = mdo
    inc (lo16 v)
    bne skip
    inc (hi16 v)
    skip <- label
    pure ()

-- | 16-bit decrement: @v--@.
dec16 :: Var16 -> ASM ()
dec16 v = mdo
    lda (lo16 v)
    bne skip
    dec (hi16 v)
    skip <- label
    dec (lo16 v)
    pure ()

-- | 16-bit unsigned compare. Sets carry for @a >= b@.
cmp16 :: Var16 -> Var16 -> ASM ()
cmp16 a b = do
    lda (lo16 a); cmp (lo16 b)
    lda (hi16 a); sbc (hi16 b)

-- | 16-bit move: @dst = src@.
mov16 :: Var16 -> Var16 -> ASM ()
mov16 dst src = do
    lda (lo16 src); sta (lo16 dst)
    lda (hi16 src); sta (hi16 dst)

-- | Load a 16-bit immediate constant into a variable: @v = imm@.
load16 :: Var16 -> Word16 -> ASM ()
load16 v imm = do
    lda # lo imm; sta (lo16 v)
    lda # hi imm; sta (hi16 v)
