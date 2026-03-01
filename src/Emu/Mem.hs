module Emu.Mem
    ( Mem, emptyMem, readByte, writeByte, loadBytes
    ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
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
