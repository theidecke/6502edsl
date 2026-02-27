module Main where

import qualified Data.ByteString as BS

import Asm.Monad (ASM, TargetConfig(..), assembleWithLabels, label)
import Asm.Mos6502
import Asm.Mos6502.Control (loop_)
import Asm.Mos6502.Debug (annotate)
import Target.C64 (c64TargetConfig, defaultC64Subsystems)
import Target.C64.Debug (exportViceLabels)
import Target.C64.Mem
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

main :: IO ()
main = do
    let cfg = c64TargetConfig 0xC000 defaultC64Subsystems
        (_, bytes, labels) = assembleWithLabels cfg program
        prg = toPRG (origin cfg) bytes
        d64 = toD64 "HELLO" prg
    BS.writeFile "hello.d64" (BS.pack d64)
    putStrLn $ "Wrote hello.d64 (" ++ show (length d64) ++ " bytes)"
    writeFile "hello.vs" (exportViceLabels labels)
    putStrLn $ "Wrote hello.vs (" ++ show (length labels) ++ " labels)"
    -- In VICE: attach disk image, then type  LOAD"*",8,1  followed by  SYS 49152

-- | Fill the screen with block characters on a black background.
program :: ASM ()
program = do
    annotate "init" $ do
        sei
        cld
        ldx # 0xFF
        txs

    annotate "setColors" $ do
        lda # colorBlack
        sta vicBorderColor
        sta vicBackgroundColor0

    annotate "fillLoop" $ do
        ldx # 0x00
        fillLoop <- label
        lda # 0xA0
        sta (screenRAM, X)
        sta (screenRAM + 0x0100, X)
        sta (screenRAM + 0x0200, X)
        sta (screenRAM + 0x0300, X)
        lda # colorLightBlue
        sta (colorRAM, X)
        sta (colorRAM + 0x0100, X)
        sta (colorRAM + 0x0200, X)
        sta (colorRAM + 0x0300, X)
        inx
        bne fillLoop

    annotate "spin" $ do
        loop_ nop
