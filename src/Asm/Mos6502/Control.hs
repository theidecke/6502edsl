{-# LANGUAGE RecursiveDo #-}

module Asm.Mos6502.Control
    ( -- * Structured if
      if_, if_eq, if_ne, if_cs, if_cc, if_pl, if_mi
      -- * Loops
    , while_, for_x, for_y, loop_
    ) where

import Data.Word (Word8)

import Asm.Monad (ASM, Label, label)
import Asm.Mos6502 (bne, beq, bcc, bcs, bpl, bmi, jmp, ldx, ldy, dex, dey, (#))

-- | Generalized if. The first argument is the branch that skips to @else@,
-- i.e. the /negation/ of the desired condition.
--
-- @if_ bne thenBlock elseBlock@ means "if equal, run thenBlock; else elseBlock."
if_ :: (Label -> ASM ()) -> ASM () -> ASM () -> ASM ()
if_ skipBranch thenBlock elseBlock = mdo
    skipBranch elseStart
    thenBlock
    jmp end
    elseStart <- label
    elseBlock
    end <- label
    pure ()

-- | If Z=1 (equal), run @then@; otherwise run @else@.
if_eq :: ASM () -> ASM () -> ASM ()
if_eq = if_ bne

-- | If Z=0 (not equal), run @then@; otherwise run @else@.
if_ne :: ASM () -> ASM () -> ASM ()
if_ne = if_ beq

-- | If C=1 (carry set / unsigned >=), run @then@; otherwise run @else@.
if_cs :: ASM () -> ASM () -> ASM ()
if_cs = if_ bcc

-- | If C=0 (carry clear / unsigned <), run @then@; otherwise run @else@.
if_cc :: ASM () -> ASM () -> ASM ()
if_cc = if_ bcs

-- | If N=0 (plus), run @then@; otherwise run @else@.
if_pl :: ASM () -> ASM () -> ASM ()
if_pl = if_ bmi

-- | If N=1 (minus), run @then@; otherwise run @else@.
if_mi :: ASM () -> ASM () -> ASM ()
if_mi = if_ bpl

-- | Parameterized while loop. @while_ beq testBlock bodyBlock@ means
-- "while test result is non-zero, execute body."
while_ :: (Label -> ASM ()) -> ASM () -> ASM () -> ASM ()
while_ exitBranch cond body = mdo
    top <- label
    cond
    exitBranch exit
    body
    jmp top
    exit <- label
    pure ()

-- | Counted loop using the X register. Runs @body@ exactly @count@ times,
-- decrementing X each iteration.
for_x :: Word8 -> ASM () -> ASM ()
for_x count body = do
    ldx # count
    top <- label
    body
    dex
    bne top

-- | Counted loop using the Y register. Runs @body@ exactly @count@ times,
-- decrementing Y each iteration.
for_y :: Word8 -> ASM () -> ASM ()
for_y count body = do
    ldy # count
    top <- label
    body
    dey
    bne top

-- | Infinite loop (common for C64 main loops).
loop_ :: ASM () -> ASM ()
loop_ body = do
    top <- label
    body
    jmp top
