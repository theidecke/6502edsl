module Asm.Monad
    ( ASM
    , TargetConfig (..)
    , emit
    , label
    , assemble
    , assembleWithLabels
    , recordLabel
    , allocZP
    , lo
    , hi
    ) where

import Control.Monad.Fix (MonadFix(..))
import Data.Bits (shiftR)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8, Word16)

-- | Target-agnostic configuration for assembling a program.
data TargetConfig = TargetConfig
    { origin       :: Word16
    , freeZeroPage :: Set Word8
    , kernalRom    :: Maybe FilePath
    , basicRom     :: Maybe FilePath
    , chargenRom   :: Maybe FilePath
    } deriving (Show)

-- | Internal assembler state: program counter + free zero-page set + labels.
data AsmState = AsmState
    { asmPC     :: !Word16
    , asmFreeZP :: !(Set Word8)
    , asmLabels :: ![(String, Word16)]
    }

-- | The assembler monad. Carries assembler state and a difference list
-- of emitted bytes.
newtype ASM a = ASM (AsmState -> (a, AsmState, Endo [Word8]))

-- | Difference list wrapper — just a newtype over @[Word8] -> [Word8]@.
newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
    mempty = Endo id

instance Functor ASM where
    fmap f (ASM g) = ASM $ \s ->
        let (a, s', w) = g s
        in  (f a, s', w)

instance Applicative ASM where
    pure a = ASM $ \s -> (a, s, mempty)
    ASM f <*> ASM g = ASM $ \s ->
        let (fab, s1, w1) = f s
            (a,   s2, w2) = g s1
        in  (fab a, s2, w1 <> w2)

instance Monad ASM where
    ASM m >>= k = ASM $ \s ->
        let (a,  s1, w1) = m s
            ASM n         = k a
            (b,  s2, w2) = n s1
        in  (b, s2, w1 <> w2)

-- | MonadFix instance — the key to forward label references.
-- Works because instruction sizes are determined eagerly (PC advances
-- immediately); only operand *values* (branch offsets, addresses) are lazy.
-- ZP allocation is also an eager state transition, so MonadFix remains correct.
instance MonadFix ASM where
    mfix f = ASM $ \s ->
        let (a, s', w) = let ASM g = f a in g s
        in  (a, s', w)

-- | Emit raw bytes and advance the program counter.
emit :: [Word8] -> ASM ()
emit bs = ASM $ \s ->
    ((), s { asmPC = asmPC s + fromIntegral (length bs) }, Endo (bs ++))

-- | Return the current program counter (defines a label).
label :: ASM Word16
label = ASM $ \s -> (asmPC s, s, mempty)

-- | Run an assembly block, returning the result, emitted bytes, and
-- collected labels (from 'recordLabel' / 'annotate').
assembleWithLabels :: TargetConfig -> ASM a -> (a, [Word8], [(String, Word16)])
assembleWithLabels cfg (ASM f) =
    let s0 = AsmState { asmPC = origin cfg, asmFreeZP = freeZeroPage cfg
                       , asmLabels = [] }
        (a, s, Endo dl) = f s0
    in  (a, dl [], reverse (asmLabels s))

-- | Run an assembly block with the given target configuration.
assemble :: TargetConfig -> ASM a -> (a, [Word8])
assemble cfg prog =
    let (a, bs, _labels) = assembleWithLabels cfg prog
    in  (a, bs)

-- | Record a named label at the given address.
-- Used by 'annotate' to collect debug symbols.
recordLabel :: String -> Word16 -> ASM ()
recordLabel name addr = ASM $ \s ->
    ((), s { asmLabels = (name, addr) : asmLabels s }, mempty)

-- | Allocate @n@ contiguous bytes from the free zero-page region.
-- Returns the start address. Fails at assembly time if no contiguous
-- block of the requested size is available.
allocZP :: Int -> ASM Word8
allocZP n
    | n <= 0    = error "allocZP: requested size must be positive"
    | otherwise = ASM $ \s ->
        let free = asmFreeZP s
            start = findContiguous n (Set.toAscList free)
            block = Set.fromList [start .. start + fromIntegral n - 1]
        in  (start, s { asmFreeZP = free `Set.difference` block }, mempty)

-- | Find @n@ contiguous bytes in a sorted list of free addresses.
findContiguous :: Int -> [Word8] -> Word8
findContiguous n = go
  where
    err = error $ "allocZP: cannot find " ++ show n
                   ++ " contiguous free zero-page bytes"

    go [] = err
    go (a:as)
        | runLen >= n = a
        -- Skip past the entire too-short contiguous run so we start
        -- searching from the next disjoint group of free addresses.
        | otherwise   = go (drop runLen (a:as))
      where
        -- Count how many addresses form a contiguous run starting at @a@.
        runLen = 1 + length (takeWhile id (zipWith (\x y -> x + 1 == y) (a:as) as))

-- | Low byte of a 16-bit word (little-endian).
lo :: Word16 -> Word8
lo = fromIntegral

-- | High byte of a 16-bit word (little-endian).
hi :: Word16 -> Word8
hi w = fromIntegral (w `shiftR` 8)
