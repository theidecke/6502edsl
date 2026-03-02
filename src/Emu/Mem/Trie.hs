{-# LANGUAGE MagicHash #-}

-- | Persistent lazy nibble trie for 64K address space.
--
-- 4-level trie keyed on nibbles of a 16-bit address, with 16-way branching
-- via 'SmallArray'. O(1) read\/write (4 hops), ~512 bytes allocation per write,
-- structural sharing across trace states.
module Emu.Mem.Trie
    ( Mem, emptyMem, readByte, writeByte, loadBytes, diffMem
    ) where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Primitive.SmallArray
import Data.Word (Word8, Word16)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | 4-level nibble trie node.
--
-- * 'Node16': 16 children in a 'SmallArray' (one per nibble value)
-- * 'Leaf': terminal value — deliberately lazy in 'Word8' to preserve thunks
--   from MonadFix assembly
-- * 'Default': entire subtree reads as 0x00; acts as implicit sparse fill
data Trie
    = Node16 {-# UNPACK #-} !(SmallArray Trie)  -- 16 children
    | Leaf Word8    -- LAZY in Word8
    | Default       -- subtree reads as 0x00

-- | 64K address space backed by a persistent nibble trie.
newtype Mem = Mem Trie

instance Show Mem where
    show (Mem _) = "<Mem>"

instance Eq Mem where
    Mem a == Mem b = trieEq a b

trieEq :: Trie -> Trie -> Bool
trieEq Default      Default      = True
trieEq (Leaf x)     (Leaf y)     = x == y
trieEq (Node16 xs)  (Node16 ys)  = all (\i -> trieEq (indexSmallArray xs i) (indexSmallArray ys i)) [0..15]
trieEq Default      (Leaf y)     = y == 0
trieEq (Leaf x)     Default      = x == 0
trieEq Default      (Node16 ys)  = all (\i -> trieEq Default (indexSmallArray ys i)) [0..15]
trieEq (Node16 xs)  Default      = all (\i -> trieEq (indexSmallArray xs i) Default) [0..15]
trieEq (Node16 xs)  (Leaf _)     = all (\i -> trieEq (indexSmallArray xs i) Default) [0..15]
trieEq (Leaf _)     (Node16 ys)  = all (\i -> trieEq Default (indexSmallArray ys i)) [0..15]

emptyMem :: Mem
emptyMem = Mem Default

-- | Extract nibble at the given level (0 = most significant, 3 = least significant).
nibble :: Word16 -> Int -> Int
nibble w level = fromIntegral ((w `shiftR` ((3 - level) * 4)) .&. 0x0F)
{-# INLINE nibble #-}

readByte :: Word16 -> Mem -> Word8
readByte addr (Mem t) = readTrie addr t
{-# INLINE readByte #-}

readTrie :: Word16 -> Trie -> Word8
readTrie addr = go 0
  where
    go _ Default      = 0
    go _ (Leaf v)     = v
    go l (Node16 arr) = go (l + 1) (indexSmallArray arr (nibble addr l))

writeByte :: Word16 -> Word8 -> Mem -> Mem
writeByte addr val (Mem t) = Mem (writeTrie addr val t)
{-# INLINE writeByte #-}

writeTrie :: Word16 -> Word8 -> Trie -> Trie
writeTrie addr val = go 0
  where
    go 4 _ = Leaf val
    go l Default =
        let idx = nibble addr l
        in  Node16 $ runSmallArray $ do
                ma <- newSmallArray 16 Default
                writeSmallArray ma idx (go (l + 1) Default)
                pure ma
    go l (Leaf _) = go l Default  -- expand misplaced leaf
    go l (Node16 arr) =
        let idx = nibble addr l
            child' = go (l + 1) (indexSmallArray arr idx)
        in  Node16 (cloneSet arr idx child')

-- | Clone a SmallArray with one element replaced.
cloneSet :: SmallArray a -> Int -> a -> SmallArray a
cloneSet arr idx val = runSmallArray $ do
    ma <- thawSmallArray arr 0 (sizeofSmallArray arr)
    writeSmallArray ma idx val
    pure ma
{-# INLINE cloneSet #-}

-- | Bulk-load a list of bytes starting at the given address.
-- Uses foldl' to force trie structure (no stack overflow) while keeping
-- leaf values lazy (preserves thunks from MonadFix assembly).
loadBytes :: Word16 -> [Word8] -> Mem -> Mem
loadBytes base bs (Mem t) = Mem (foldl' go t (zip [base ..] bs))
  where
    go trie (a, v) = writeTrie a v trie

-- | Compare two memory states, returning (address, old, new) for all differing bytes.
-- Uses pointer equality to skip shared subtrees (the common case after copy-on-write).
diffMem :: Mem -> Mem -> [(Word16, Word8, Word8)]
diffMem (Mem old) (Mem new) = diffTrie 0 0 old new

-- | Pointer equality test. Conservative: False doesn't mean different values.
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}

leafVal :: Trie -> Word8
leafVal (Leaf v)   = v
leafVal Default    = 0
leafVal (Node16 _) = 0

childOf :: Trie -> Int -> Trie
childOf (Node16 arr) i = indexSmallArray arr i
childOf _            _ = Default

-- | Walk both tries in parallel, collecting differences.
-- Skips pointer-equal subtrees (the common case with copy-on-write sharing).
diffTrie :: Word16 -> Int -> Trie -> Trie -> [(Word16, Word8, Word8)]
diffTrie _ _ old new | ptrEq old new = []
diffTrie prefix 4 old new =
    let o = leafVal old; n = leafVal new
    in  if o == n then [] else [(prefix, o, n)]
diffTrie _ _ Default Default = []
diffTrie prefix level old new =
    concatMap (\i ->
        diffTrie (prefix .|. (fromIntegral i `shiftL` ((3 - level) * 4)))
                 (level + 1) (childOf old i) (childOf new i))
        [0..15]
