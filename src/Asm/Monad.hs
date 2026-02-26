module Asm.Monad
    ( ASM
    , emit
    , label
    , assemble
    , lo
    , hi
    ) where

import Control.Monad.Fix (MonadFix(..))
import Data.Bits (shiftR)
import Data.Word (Word8, Word16)

-- | The assembler monad. Takes a program counter, returns a result,
-- the updated program counter, and a difference list of emitted bytes.
newtype ASM a = ASM (Word16 -> (a, Word16, Endo [Word8]))

-- | Difference list wrapper — just a newtype over @[Word8] -> [Word8]@.
newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
    mempty = Endo id

instance Functor ASM where
    fmap f (ASM g) = ASM $ \pc ->
        let (a, pc', w) = g pc
        in  (f a, pc', w)

instance Applicative ASM where
    pure a = ASM $ \pc -> (a, pc, mempty)
    ASM f <*> ASM g = ASM $ \pc ->
        let (fab, pc1, w1) = f pc
            (a,   pc2, w2) = g pc1
        in  (fab a, pc2, w1 <> w2)

instance Monad ASM where
    ASM m >>= k = ASM $ \pc ->
        let (a,  pc1, w1) = m pc
            ASM n          = k a
            (b,  pc2, w2) = n pc1
        in  (b, pc2, w1 <> w2)

-- | MonadFix instance — the key to forward label references.
-- Works because instruction sizes are determined eagerly (PC advances
-- immediately); only operand *values* (branch offsets, addresses) are lazy.
instance MonadFix ASM where
    mfix f = ASM $ \pc ->
        let (a, pc', w) = let ASM g = f a in g pc
        in  (a, pc', w)

-- | Emit raw bytes and advance the program counter.
emit :: [Word8] -> ASM ()
emit bs = ASM $ \pc ->
    ((), pc + fromIntegral (length bs), Endo (bs ++))

-- | Return the current program counter (defines a label).
label :: ASM Word16
label = ASM $ \pc -> (pc, pc, mempty)

-- | Run an assembly block starting at the given origin address.
assemble :: Word16 -> ASM a -> (a, [Word8])
assemble org (ASM f) =
    let (a, _pc, Endo dl) = f org
    in  (a, dl [])

-- | Low byte of a 16-bit word (little-endian).
lo :: Word16 -> Word8
lo = fromIntegral

-- | High byte of a 16-bit word (little-endian).
hi :: Word16 -> Word8
hi w = fromIntegral (w `shiftR` 8)

