{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Word (Word8, Word16)
import Numeric (showHex)
import qualified Data.ByteString as BS

import Asm.Monad (TargetConfig(..), assemble, label)
import Asm.Mos6502
import Asm.Mos6502.Control (for_x, if_eq, loop_)
import Asm.Mos6502.Ops16 (load16, inc16)
import Target.C64 (c64TargetConfig, defaultC64Subsystems, C64Subsystems(..))
import Target.C64.Data (byte, word, petscii)
import Target.C64.Mem (vicBorderColor, vicBackgroundColor0, colorBlack)
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

main :: IO ()
main = do
    let cfg = c64TargetConfig 0xC000 defaultC64Subsystems

    -- Test 1: Init + set border/background colors, then spin forever
    let (_, bytes1) = assemble cfg $ do
            sei
            cld
            ldx # 0xFF
            txs
            lda # colorBlack
            sta vicBorderColor
            sta vicBackgroundColor0
            loop <- label
            jmp loop

    putStrLn "Test 1 — init + set colors + infinite loop:"
    putStrLn $ "  " ++ hexDump bytes1
    putStrLn $ "  expected: 78 D8 A2 FF 9A A9 00 8D 20 D0 8D 21 D0 4C 0D C0"
    putStrLn ""

    -- Test 2: Forward branch via mdo
    -- LDA $10; BEQ skip; LDA #$FF; skip: RTS
    let (_, bytes2) = assemble cfg $ mdo
            lda (0x10 :: Word8)
            beq skip
            lda # 0xFF
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
            lda # 0x42
            lda (0x80 :: Word8)
            lda (0x80 :: Word8, X)
            lda (0x1234 :: Word16)
            lda (0x1234 :: Word16, X)
            lda (0x1234 :: Word16, Y)
            lda ((0x80 :: Word8) ! X)
            lda ((0x80 :: Word8) ! Y)
    putStrLn "Test 4 — LDA all 8 addressing modes:"
    putStrLn $ "  " ++ hexDump bytes4
    putStrLn $ "  expected: A9 42 A5 80 B5 80 AD 34 12 BD 34 12 B9 34 12 A1 80 B1 80"
    putStrLn ""

    -- Test 5: Zero-page allocation with C64 target config (BASIC off)
    let zpCfg = c64TargetConfig 0x0800 defaultC64Subsystems
                    { useBasic = False }
    let (_, bytes5) = assemble zpCfg $ mdo
            -- Allocate a pointer (2 ZP bytes)
            ptr <- allocPtr
            -- Use the allocated ZP address
            lda # 0x00
            sta ptr
            sta (ptr + 1)
            -- Load through the pointer
            lda (ptr ! Y)
            rts
    putStrLn "Test 5 — zero-page allocation (C64, BASIC off):"
    putStrLn $ "  " ++ hexDump bytes5
    putStrLn ""

    -- Test 6: Data embedding (byte, word, petscii)
    let (_, bytes6) = assemble cfg $ do
            byte [0x01, 0x02, 0x03]
            word [0x1234, 0x5678]
            petscii "HI"
    putStrLn "Test 6 — data embedding (byte, word, petscii):"
    putStrLn $ "  " ++ hexDump bytes6
    putStrLn $ "  expected: 01 02 03 34 12 78 56 48 49"
    putStrLn ""

    -- Test 7: Control flow (for_x)
    let (_, bytes7) = assemble cfg $ do
            for_x 0x0A $ do
                nop
    putStrLn "Test 7 — control flow (for_x):"
    putStrLn $ "  " ++ hexDump bytes7
    putStrLn $ "  expected: A2 0A EA CA D0 FC"
    putStrLn ""

    -- Test 8: 16-bit ops (load16, inc16)
    let zpCfg2 = c64TargetConfig 0x0800 defaultC64Subsystems
                    { useBasic = False }
    let (_, bytes8) = assemble zpCfg2 $ do
            v <- allocVar16
            load16 v 0x1234
            inc16 v
    putStrLn "Test 8 — 16-bit ops (load16, inc16):"
    putStrLn $ "  " ++ hexDump bytes8
    putStrLn $ "  expected: A9 34 85 02 A9 12 85 03 E6 02 D0 02 E6 03"
    putStrLn ""

    -- Test 9: Structured if (if_eq with cmp)
    let (_, bytes9) = assemble cfg $ do
            lda # 0x05
            cmp # 0x05
            if_eq
                (lda # 0x01)
                (lda # 0x02)
            rts
    putStrLn "Test 9 — structured if (if_eq):"
    putStrLn $ "  " ++ hexDump bytes9
    putStrLn $ "  expected: A9 05 C9 05 D0 05 A9 01 4C 0D C0 A9 02 60"
    putStrLn ""

    -- Test 10: Infinite loop (loop_)
    let (_, bytes10) = assemble cfg $ do
            loop_ $ do
                inc (0x80 :: Word8)
    putStrLn "Test 10 — infinite loop (loop_):"
    putStrLn $ "  " ++ hexDump bytes10
    putStrLn $ "  expected: E6 80 4C 00 C0"
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
