{-# LANGUAGE RecursiveDo #-}

module Asm.Mos6502.Control
    ( -- * Two-armed if
      if_, if_eq, if_ne, if_cs, if_cc, if_pl, if_mi
      -- * One-armed when (no else branch, tighter code)
    , when_, when_eq, when_ne, when_cs, when_cc, when_pl, when_mi
      -- * Loops
    , while_, for_x, for_y, loop_
    ) where

import Data.Word (Word8)

import Asm.Monad (MonadASM, Label, label)
import Asm.Mos6502 (bne, beq, bcc, bcs, bpl, bmi, jmp, ldx, ldy, dex, dey, (#))

-- | Generalized if. The first argument is the branch that skips to @else@,
-- i.e. the /negation/ of the desired condition.
--
-- @if_ bne thenBlock elseBlock@ means "if equal, run thenBlock; else elseBlock."
if_ :: MonadASM m => (Label -> m ()) -> m () -> m () -> m ()
if_ skipBranch thenBlock elseBlock = mdo
    skipBranch elseStart
    thenBlock
    jmp end
    elseStart <- label
    elseBlock
    end <- label
    pure ()

-- | If Z=1 (equal), run @then@; otherwise run @else@.
if_eq :: MonadASM m => m () -> m () -> m ()
if_eq = if_ bne

-- | If Z=0 (not equal), run @then@; otherwise run @else@.
if_ne :: MonadASM m => m () -> m () -> m ()
if_ne = if_ beq

-- | If C=1 (carry set / unsigned >=), run @then@; otherwise run @else@.
if_cs :: MonadASM m => m () -> m () -> m ()
if_cs = if_ bcc

-- | If C=0 (carry clear / unsigned <), run @then@; otherwise run @else@.
if_cc :: MonadASM m => m () -> m () -> m ()
if_cc = if_ bcs

-- | If N=0 (plus), run @then@; otherwise run @else@.
if_pl :: MonadASM m => m () -> m () -> m ()
if_pl = if_ bmi

-- | If N=1 (minus), run @then@; otherwise run @else@.
if_mi :: MonadASM m => m () -> m () -> m ()
if_mi = if_ bpl

-- | One-armed if. The first argument is the branch that skips past the body,
-- i.e. the /negation/ of the desired condition.
--
-- Generates @branch end; body; end:@ — no JMP, tighter than 'if_' with an
-- empty else.
when_ :: MonadASM m => (Label -> m ()) -> m () -> m ()
when_ skipBranch body = mdo
    skipBranch end
    body
    end <- label
    pure ()

-- | Execute body when Z=1 (equal / zero).
when_eq :: MonadASM m => m () -> m ()
when_eq = when_ bne

-- | Execute body when Z=0 (not equal / non-zero).
when_ne :: MonadASM m => m () -> m ()
when_ne = when_ beq

-- | Execute body when C=1 (carry set / unsigned >=).
when_cs :: MonadASM m => m () -> m ()
when_cs = when_ bcc

-- | Execute body when C=0 (carry clear / unsigned <).
when_cc :: MonadASM m => m () -> m ()
when_cc = when_ bcs

-- | Execute body when N=0 (plus / positive).
when_pl :: MonadASM m => m () -> m ()
when_pl = when_ bmi

-- | Execute body when N=1 (minus / negative).
when_mi :: MonadASM m => m () -> m ()
when_mi = when_ bpl

-- | Parameterized while loop. @while_ beq testBlock bodyBlock@ means
-- "while test result is non-zero, execute body."
while_ :: MonadASM m => (Label -> m ()) -> m () -> m () -> m ()
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
for_x :: MonadASM m => Word8 -> m () -> m ()
for_x count body = do
    ldx # count
    top <- label
    body
    dex
    bne top

-- | Counted loop using the Y register. Runs @body@ exactly @count@ times,
-- decrementing Y each iteration.
for_y :: MonadASM m => Word8 -> m () -> m ()
for_y count body = do
    ldy # count
    top <- label
    body
    dey
    bne top

-- | Infinite loop (common for C64 main loops).
loop_ :: MonadASM m => m () -> m ()
loop_ body = do
    top <- label
    body
    jmp top
