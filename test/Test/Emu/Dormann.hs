module Test.Emu.Dormann (tests) where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as BS

import Test.Helpers (checkIO, section)
import Emu.CPU
import Emu.Mem (loadBytes, emptyMem)
import Emu.Step (step)

tests :: [IO Bool]
tests =
    [ section "Emu.Dormann (Klaus Dormann functional test)"
    , checkIO "6502 functional test reaches $3469" runDormann
    ]

-- | Load the 65536-byte binary at $0000, set PC to $0400, run until
-- PC is stuck (same PC two consecutive steps) or cycle limit.
-- Success if final PC == 0x3469 (the success halt: JMP *).
runDormann :: IO Bool
runDormann = do
    bin <- BS.readFile "test/data/6502_functional_test.bin"
    let bytes = BS.unpack bin
        m     = loadBytes 0x0000 bytes emptyMem
        s0    = initCPU { cpuMem = m, cpuPC = 0x0400 }
        maxCyc = 100000000 :: Int  -- 100M cycle limit
        go !prev !cur
            | view regPC cur == view regPC prev = cur  -- stuck
            | view cycles cur > maxCyc           = cur  -- timeout
            | otherwise                          = go cur (step cur)
        s1 = step s0
        final = go s0 s1
        pc = view regPC final
        cyc = view cycles final
    if pc == 0x3469
        then do putStr (" (" ++ show cyc ++ " cycles) ")
                pure True
        else do putStr (" (stuck at PC=$" ++ showHex16 pc ++ " after " ++ show cyc ++ " cycles) ")
                pure False

showHex16 :: Word16 -> String
showHex16 w = concatMap hexByte [fromIntegral (w `div` 256), fromIntegral (w `mod` 256)]
  where
    hexByte :: Word8 -> String
    hexByte b = [hexDigit (b `div` 16), hexDigit (b `mod` 16)]
    hexDigit n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
               | otherwise = toEnum (fromEnum 'A' + fromIntegral n - 10)
