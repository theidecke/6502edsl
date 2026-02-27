{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Word (Word8)
import qualified Data.ByteString as BS

import Asm.Monad (ASM, TargetConfig(..), assembleWithLabels, label, lo, hi)
import Asm.Mos6502
import Asm.Mos6502.Control (if_eq, while_, for_y, loop_)
import Asm.Mos6502.Debug (fitsIn, annotate)
import Asm.Mos6502.Memory (alignPage, samePage)
import Asm.Mos6502.Ops16 (load16, inc16)
import Target.C64 (C64Subsystems(..), c64TargetConfig, defaultC64Subsystems)
import Target.C64.Data (byte, petscii)
import Target.C64.Debug (exportViceLabels)
import Target.C64.Mem
import Target.C64.PRG (toPRG)
import Target.C64.D64 (toD64)

-- | Color-wash demo for the C64.
--
-- Clears the screen, prints a centered PETSCII banner, initializes SID
-- voice 1 for a continuous sawtooth tone, then runs a main loop that:
--
--   * Waits for vertical blank (raster line 251)
--   * Color-washes all 1000 bytes of color RAM using a rotating phase
--     offset into a 16-color palette table
--   * Toggles the SID frequency every 16 frames for a warble effect
--   * Increments a 16-bit frame counter
--
-- In VICE: attach disk image, then type  LOAD"*",8,1  followed by  SYS 49152
main :: IO ()
main = do
    let subs = defaultC64Subsystems { useBasic = False }
        cfg  = c64TargetConfig 0xC000 subs
        (_, bytes, labels) = assembleWithLabels cfg program
        prg = toPRG (origin cfg) bytes
        d64 = toD64 "COLOR WASHER" prg
    BS.writeFile "color-washer.d64" (BS.pack d64)
    putStrLn $ "Wrote color-washer.d64 (" ++ show (length d64) ++ " bytes)"
    writeFile "color-washer.vs" (exportViceLabels labels)
    putStrLn $ "Wrote color-washer.vs (" ++ show (length labels) ++ " labels)"

program :: ASM ()
program = mdo
    -- Allocate typed zero-page variables (BASIC is disabled → plenty of ZP)
    colorPhase <- allocVar8
    frameCount <- allocVar16
    screenPtr  <- allocPtr

    --------------------------------------------------------------------------
    -- Init: disable interrupts, set up stack
    --------------------------------------------------------------------------
    annotate "init" $ do
        sei
        cld
        ldx # 0xFF
        txs

    --------------------------------------------------------------------------
    -- Set border and background to black
    --------------------------------------------------------------------------
    annotate "setupScreen" $ do
        lda # colorBlack
        sta vicBorderColor
        sta vicBackgroundColor0

    --------------------------------------------------------------------------
    -- Clear screen (spaces) and color RAM (black)
    --------------------------------------------------------------------------
    annotate "clearScreen" $ do
        ldx # 0x00
        clearLoop <- label
        lda # 0x20
        sta (screenRAM, X)
        sta (screenRAM + 0x0100, X)
        sta (screenRAM + 0x0200, X)
        sta (screenRAM + 0x0300, X)
        lda # colorBlack
        sta (colorRAM, X)
        sta (colorRAM + 0x0100, X)
        sta (colorRAM + 0x0200, X)
        sta (colorRAM + 0x0300, X)
        inx
        bne clearLoop

    --------------------------------------------------------------------------
    -- Clear all SID registers
    --------------------------------------------------------------------------
    annotate "clearSID" $ do
        ldx # 0x18
        lda # 0x00
        clearSIDLoop <- label
        sta (sidIOBase, X)
        dex
        bpl clearSIDLoop

    --------------------------------------------------------------------------
    -- Init SID voice 1: sawtooth wave, medium attack, full sustain
    --------------------------------------------------------------------------
    annotate "initSID" $ do
        lda # 0x09              -- Attack=0, Decay=9
        sta sidV1AttackDecay
        lda # 0xA0              -- Sustain=10, Release=0
        sta sidV1SustainRelease
        lda # 0x00
        sta sidV1FreqLo
        lda # 0x1C              -- ~C-4 note
        sta sidV1FreqHi
        lda # 0x21              -- Sawtooth waveform + gate on
        sta sidV1Control
        lda # 0x0F              -- Volume max, no filter
        sta sidVolumeFilterMode

    --------------------------------------------------------------------------
    -- Print centered banner message
    -- load16 sets up a 16-bit pointer; for_y counts down from msgLen to 1.
    -- Data at msgData is PETSCII; AND #$3F converts to screen codes.
    --------------------------------------------------------------------------
    annotate "printMessage" $ do
        let msgAddr = screenRAM + 12 * 40 + 13
        lda # lo msgAddr; sta screenPtr
        lda # hi msgAddr; sta (screenPtr + 1)
        for_y msgLen $ do
            lda (msgData, Y)
            and_ # 0x3F        -- PETSCII → screen code
            sta (screenPtr ! Y)

    --------------------------------------------------------------------------
    -- Zero out runtime variables
    --------------------------------------------------------------------------
    annotate "initVars" $ do
        lda # 0x00
        sta colorPhase
        load16 frameCount 0

    --------------------------------------------------------------------------
    -- Main loop (infinite via loop_)
    --------------------------------------------------------------------------
    annotate "mainLoop" $ loop_ $ do

        -- Wait for vertical blank: raster line 251
        annotate "waitVSync" $ do
            while_ beq (do lda vicRasterLine; cmp # 251) nop
            while_ bne (do lda vicRasterLine; cmp # 251) nop

        -- Color-wash all of color RAM ($D800-$DBFF)
        annotate "colorWash" $ do
            ldx # 0x00
            washLoop <- label
            fitsIn 32 $ do
                txa
                clc
                adc colorPhase
                and_ # 0x0F
                tay
                lda (colorTable, Y)
                sta (colorRAM, X)
                sta (colorRAM + 0x0100, X)
                sta (colorRAM + 0x0200, X)
                sta (colorRAM + 0x0300, X)
                inx
                bne washLoop

        -- Advance color phase for next frame
        annotate "updatePhase" $ do
            inc colorPhase

        -- Warble: toggle SID frequency every 16 frames
        annotate "updateSID" $ do
            lda (lo16 frameCount)
            and_ # 0x10
            if_eq
                (do lda # 0x24; sta sidV1FreqHi)   -- high pitch
                (do lda # 0x1C; sta sidV1FreqHi)   -- low pitch

        -- Increment 16-bit frame counter
        annotate "updateFrame" $ do
            inc16 frameCount

    --------------------------------------------------------------------------
    -- Data section
    --------------------------------------------------------------------------

    -- Page-align the color table so Y-indexed reads stay in one page
    alignPage
    colorTable <- label
    annotate "colorTable" $ do
        byte [ colorBlack,  colorDarkGrey,  colorBrown,    colorRed
             , colorLightRed, colorOrange,  colorYellow,   colorLightGreen
             , colorGreen,  colorCyan,      colorLightBlue, colorBlue
             , colorPurple, colorMediumGrey, colorLightGrey, colorWhite
             ]
    samePage colorTable

    -- Message data: 1-indexed (byte 0 is padding for for_y's Y=count..1)
    msgData <- label
    annotate "msgData" $ do
        byte [0x00]
        petscii "COLOR WASHER"

    pure ()
  where
    msgLen = 12 :: Word8
