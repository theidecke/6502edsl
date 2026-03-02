module Emu.Mem.IntMap
    ( Mem, emptyMem, readByte, writeByte, loadBytes, diffMem
    ) where

import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IM
import Data.Word (Word8, Word16)

-- | 64K address space backed by a sparse IntMap. Uninitialized reads return 0.
newtype Mem = Mem (IntMap Word8)
    deriving (Show, Eq)

emptyMem :: Mem
emptyMem = Mem IM.empty

readByte :: Word16 -> Mem -> Word8
readByte addr (Mem m) = IM.findWithDefault 0 (fromIntegral addr) m

writeByte :: Word16 -> Word8 -> Mem -> Mem
writeByte addr val (Mem m) = Mem (IM.insert (fromIntegral addr) val m)

-- | Bulk-load a list of bytes starting at the given address.
loadBytes :: Word16 -> [Word8] -> Mem -> Mem
loadBytes base bs (Mem m) = Mem (foldl' go m (zip [fromIntegral base ..] bs))
  where
    go im (a, v) = IM.insert a v im

-- | Compare two memory states, returning (address, old, new) for all differing bytes.
diffMem :: Mem -> Mem -> [(Word16, Word8, Word8)]
diffMem (Mem old) (Mem new) =
    map (\(a, (o, n)) -> (fromIntegral a, o, n)) $ IM.toAscList merged
  where
    -- Keys in both maps: keep only where values differ
    both _k o n = if o == n then Nothing else Just (o, n)
    -- Keys only in old: old value vs 0
    onlyOld = IM.map (\o -> (o, 0))
    -- Keys only in new: 0 vs new value
    onlyNew = IM.map (\n -> (0, n))
    merged = IM.mergeWithKey both onlyOld onlyNew old new
