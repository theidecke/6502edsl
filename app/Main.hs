{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Word (Word8)
import Numeric (showHex)
import qualified Data.ByteString as BS

import Asm.Monad (TargetConfig(..), assemble, allocZP, label)
import Asm.Mos6502
import Target.C64 (c64TargetConfig, defaultC64Subsystems, C64Subsystems(..))
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

main :: IO ()
main = do
    let cfg = c64TargetConfig 0x0800 defaultC64Subsystems

    -- Test 1: Linear program — init + set border/background colors
    -- SEI; CLD; LDX #$FF; TXS; LDA #$00; STA $D020; STA $D021; RTS
    let (_, bytes1) = assemble cfg $ do
            sei
            cld
            ldx (Imm 0xFF)
            txs
            lda (Imm 0x00)
            sta (Abs 0xD020)
            sta (Abs 0xD021)
            rts

    putStrLn "Test 1 — linear program (init + set colors):"
    putStrLn $ "  " ++ hexDump bytes1
    putStrLn $ "  expected: 78 D8 A2 FF 9A A9 00 8D 20 D0 8D 21 D0 60"
    putStrLn ""

    -- Test 2: Forward branch via mdo
    -- LDA $10; BEQ skip; LDA #$FF; skip: RTS
    let (_, bytes2) = assemble cfg $ mdo
            lda (ZP 0x10)
            beq skip
            lda (Imm 0xFF)
            skip <- label
            rts
    putStrLn "Test 2 — forward branch (mdo):"
    putStrLn $ "  " ++ hexDump bytes2
    putStrLn $ "  expected: A5 10 F0 02 A9 FF 60"
    putStrLn ""

    -- Test 3: Backward branch (loop)
    -- loop: DEX; BNE loop
    let (_, bytes3) = assemble cfg $ do
            loop <- label
            dex
            bne loop
    putStrLn "Test 3 — backward branch (loop):"
    putStrLn $ "  " ++ hexDump bytes3
    putStrLn $ "  expected: CA D0 FD"
    putStrLn ""

    -- Test 4: LDA with all 8 addressing modes
    let (_, bytes4) = assemble cfg $ do
            lda (Imm  0x42)
            lda (ZP   0x80)
            lda (ZPX  0x80)
            lda (Abs  0x1234)
            lda (AbsX 0x1234)
            lda (AbsY 0x1234)
            lda (IndX 0x80)
            lda (IndY 0x80)
    putStrLn "Test 4 — LDA all 8 addressing modes:"
    putStrLn $ "  " ++ hexDump bytes4
    putStrLn $ "  expected: A9 42 A5 80 B5 80 AD 34 12 BD 34 12 B9 34 12 A1 80 B1 80"
    putStrLn ""

    -- Test 5: Zero-page allocation with C64 target config (BASIC off)
    let zpCfg = c64TargetConfig 0x0800 defaultC64Subsystems
                    { useBasic = False }
    let (_, bytes5) = assemble zpCfg $ mdo
            -- Allocate 2 ZP bytes for a pointer
            ptr <- allocZP 2
            -- Use the allocated ZP address
            lda (Imm 0x00)
            sta (ZP ptr)
            sta (ZP (ptr + 1))
            -- Load through the pointer
            lda (IndY ptr)
            rts
    putStrLn "Test 5 — zero-page allocation (C64, BASIC off):"
    putStrLn $ "  " ++ hexDump bytes5
    putStrLn ""

    -- Write Test 1 program as a D64 disk image
    let prg     = toPRG (origin cfg) bytes1
        d64     = toD64 "SETCOLORS" prg
        d64File = "setcolors.d64"
    BS.writeFile d64File (BS.pack d64)
    putStrLn $ "Wrote " ++ d64File ++ " (" ++ show (length d64) ++ " bytes)"

hexDump :: [Word8] -> String
hexDump = unwords . map (\b -> pad2 (showHex b ""))
  where
    pad2 [c] = ['0', c]
    pad2 s   = s
