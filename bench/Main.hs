module Main (main) where

import Data.Word (Word8, Word16)
import Test.Tasty.Bench

import Emu.Mem.IntMap qualified as IM
import Emu.Mem.Trie   qualified as Trie

main :: IO ()
main = defaultMain
    [ bgroup "writeByte-single"
        [ bench "IntMap" $ whnf (IM.writeByte 0x1234 0x42)   IM.emptyMem
        , bench "Trie"   $ whnf (Trie.writeByte 0x1234 0x42) Trie.emptyMem
        ]
    , bgroup "writeByte-1000"
        [ bench "IntMap" $ whnf writeMany_IM   IM.emptyMem
        , bench "Trie"   $ whnf writeMany_Trie Trie.emptyMem
        ]
    , bgroup "readByte-miss"
        [ bench "IntMap" $ nf (`IM.readByte`   IM.emptyMem)   (0x1234 :: Word16)
        , bench "Trie"   $ nf (`Trie.readByte` Trie.emptyMem) (0x1234 :: Word16)
        ]
    , bgroup "readByte-hit"
        [ bench "IntMap" $ nf (`IM.readByte`   imPopulated)   (0x0500 :: Word16)
        , bench "Trie"   $ nf (`Trie.readByte` triePopulated) (0x0500 :: Word16)
        ]
    , bgroup "loadBytes-65536"
        [ bench "IntMap" $ whnf (IM.loadBytes 0 fullBlock)   IM.emptyMem
        , bench "Trie"   $ whnf (Trie.loadBytes 0 fullBlock) Trie.emptyMem
        ]
    , bgroup "readByte-256-after-load"
        [ bench "IntMap" $ nf (readRange_IM imLoaded)   (0x0800 :: Word16)
        , bench "Trie"   $ nf (readRange_Trie trieLoaded) (0x0800 :: Word16)
        ]
    , bgroup "diffMem-identical"
        [ bench "IntMap" $ nf (IM.diffMem   imPopulated)   imPopulated
        , bench "Trie"   $ nf (Trie.diffMem triePopulated) triePopulated
        ]
    , bgroup "diffMem-one-write"
        [ bench "IntMap" $ nf (IM.diffMem   imPopulated)   imOneWrite
        , bench "Trie"   $ nf (Trie.diffMem triePopulated) trieOneWrite
        ]
    ]

-- Helpers

writeMany_IM :: IM.Mem -> IM.Mem
writeMany_IM m0 = foldl' (\m i -> IM.writeByte (fromIntegral i) (fromIntegral i) m) m0 [0 :: Int .. 999]

writeMany_Trie :: Trie.Mem -> Trie.Mem
writeMany_Trie m0 = foldl' (\m i -> Trie.writeByte (fromIntegral i) (fromIntegral i) m) m0 [0 :: Int .. 999]

fullBlock :: [Word8]
fullBlock = map fromIntegral [0 :: Int .. 65535]

-- Pre-populated memories with 1000 bytes
imPopulated :: IM.Mem
imPopulated = writeMany_IM IM.emptyMem

triePopulated :: Trie.Mem
triePopulated = writeMany_Trie Trie.emptyMem

-- Fully loaded memories (65536 bytes)
imLoaded :: IM.Mem
imLoaded = IM.loadBytes 0 fullBlock IM.emptyMem

trieLoaded :: Trie.Mem
trieLoaded = Trie.loadBytes 0 fullBlock Trie.emptyMem

-- One-write variants for diffMem
imOneWrite :: IM.Mem
imOneWrite = IM.writeByte 0x0500 0xFF imPopulated

trieOneWrite :: Trie.Mem
trieOneWrite = Trie.writeByte 0x0500 0xFF triePopulated

readRange_IM :: IM.Mem -> Word16 -> Word8
readRange_IM m base = foldl' (\acc i -> acc + IM.readByte (base + i) m) 0 [0..255]

readRange_Trie :: Trie.Mem -> Word16 -> Word8
readRange_Trie m base = foldl' (\acc i -> acc + Trie.readByte (base + i) m) 0 [0..255]
