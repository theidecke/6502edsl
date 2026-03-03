{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BinaryLiterals #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)
import Data.Bits (shiftL, complement, testBit, (.|.))
import Data.Word (Word8, Word16)
import Numeric (showFFloat)

import Asm.Monad (MonadASM, MonadZPAlloc, Label, TargetConfig(..), ToAddr(..), assembleWithLabels, label, lo, hi)
import Backend.ACME (exportAcmeWith)
import Asm.Mos6502
import Asm.Mos6502.Control (if_ne, if_cs, when_eq, when_ne, for_x, for_y)
import Asm.Mos6502.Debug (annotate, fitsIn)
import Asm.Mos6502.Memory (alignPage, samePage)
import Backend.C64.D64 (toD64)
import Backend.C64.PRG (toPRG)
import Backend.C64.ViceLabels (exportViceLabels)
import Emu.CPU (CPUState, initCPU, view, set, regPC, memAt, cycles)
import Emu.Trace (loadProgram, traceForCycles, pcCoverage, formatProfile, formatState)
import Target.C64 (C64Subsystems(..), c64TargetConfig, defaultC64Subsystems, loadC64Roms)
import Target.C64.Data (byte)
import Target.C64.Mem
import Target.C64.RomLabels (romLabels, romMOVMF, romMOVFM, romFACINX,
    romFADD, romFSUB, romFDIV, romFMULT, romGIVAYF, romMOVEF, romMOVFA,
    romFSUBT, romQINT, romCONUPK)

-- =========================================================================
-- Non-ZP data addresses (Attractor computation + drawing)
-- =========================================================================

fpA, fpB, fpC :: Word16
fpA = 0xC400
fpB = 0xC430
fpC = 0xC460

fpXcur, fpYcur, fpZcur :: Word16
fpXcur = 0xC4C0  -- 5 bytes per float
fpYcur = 0xC4F0
fpZcur = 0xC520

fpTemp, fpTemp2, fp10, fpScaleY, fpOffsetX :: Word16
fpTemp    = 0xC550
fpTemp2   = 0xC555
fp10      = 0xC55A
fpScaleY  = 0xC560
fpOffsetX = 0xC570

intX, intY, intM :: Word16
intX = 0xC600
intY = 0xC602
intM = 0xC604

intXdt, intYdt, intZdt :: Word16
intXdt = 0xC606
intYdt = 0xC608
intZdt = 0xC60A

scrAddr :: Word16
scrAddr = 0xC630

-- X offset where the attractor is visually split in two halves;
-- used for implementing the left and right sound switching
-- xSeparator = 164

-- LUT for ORing and ANDing pixel patterns
screenMaskOr0, screenMaskAnd0 :: Word16
screenMaskOr0  = 0xC640
screenMaskAnd0 = 0xC648

-- Set to 1 to use fast mult.
useFastMult :: Word16
useFastMult = 0xC800

testMem :: Word16
testMem = 0xC650

-- =========================================================================
-- FAC/ARG internal zero-page addresses (hardcoded, NOT allocated)
--
-- These are safe from allocator conflicts: with useBasic = True, the range
-- $03-$8F is reserved (not in freeZeroPage), so the allocator will never
-- assign these addresses.
-- =========================================================================

-- FAC1
--   exponent is in $61
--   mantissa is in $62 $63 $64 $65
--   sign is in $66
--   possibly clear $70
facExp, facMant0, facMant1, facMant2, facMant3, facSign :: Word8
facExp   = 0x61
facMant0 = 0x62
facMant1 = 0x63
facMant2 = 0x64
facMant3 = 0x65
facSign  = 0x66

-- ARG
--   exponent is in $69
--   mantissa is in $6A $6B $6C $6D
--   sign is in $6E (0 for positive, $FF (-1) for negative)
argExp, argMant0, argMant1, argMant2, argMant3, argSign :: Word8
argExp   = 0x69
argMant0 = 0x6A
argMant1 = 0x6B
argMant2 = 0x6C
argMant3 = 0x6D
argSign  = 0x6E

facOverflow :: Word8
facOverflow = 0x70

-- =========================================================================
-- Constants
-- =========================================================================

-- Normally we would multiply delta-time (dt, e.g. 0.01) to our differential
-- equation results to get the next X/Y/Z values. We can save a multiplication
-- here if we assume that we approximate 0.01 by shifting the float exponent
-- to the right.
--
-- Some approximate values:
-- - 1/(2**8) = 0.0039..
-- - 1/(2**7) = 0.0078..
-- - 1/(2**6) = 0.0156..
--
-- So we just define the shift amount to set our dt.
dtShift :: Word8
dtShift = 6

-- =========================================================================
-- Helper functions (assembly macros → Haskell)
-- =========================================================================

-- | Load FAC1 from memory: +mem_to_fac1
memToFac1 :: MonadASM m => Word16 -> m ()
memToFac1 name = do
    lda # lo name
    ldy # hi name
    jsr romMOVFM

-- | Store FAC1 to memory: +movmf / +fac1_to_mem
storeFac :: MonadASM m => Word16 -> m ()
storeFac name = do
    ldx # lo name
    ldy # hi name
    jsr romMOVMF

-- | FAC1 = FAC1 + Mem: +fadd
fpAdd :: MonadASM m => Word16 -> m ()
fpAdd other = do
    lda # lo other
    ldy # hi other
    jsr romFADD

-- | FAC1 = Mem - FAC1: +fsub
fpSub :: MonadASM m => Word16 -> m ()
fpSub other = do
    lda # lo other
    ldy # hi other
    jsr romFSUB

-- | FAC1 = Mem / FAC1: +fdiv
fpDiv :: MonadASM m => Word16 -> m ()
fpDiv other = do
    lda # lo other
    ldy # hi other
    jsr romFDIV

-- | Multiply number from RAM * FAC1, choosing fast_mult or ROM FMULT
-- based on USE_FAST_MULT flag. Takes fast_mult label as parameter.
fpMul :: MonadASM m => Word16 -> Label -> m ()
fpMul other fastMultLbl = mdo
    lda # lo other
    ldy # hi other
    pha
    lda # 1
    cmp useFastMult
    beq useFast
    pla
    jsr romFMULT
    jmp done
    useFast <- label
    pla
    jsr fastMultLbl
    done <- label
    pure ()

-- | Initialize float at addr to 16-bit int value: +set_int_param
setIntParam :: MonadASM m => Word16 -> Word8 -> m ()
setIntParam name value = do
    ldy # value
    lda # 0
    jsr romGIVAYF
    ldx # lo name
    ldy # hi name
    jsr romMOVMF

-- | Convert FAC1 to 16-bit signed int and store: +fac1_to_int16
fac1ToInt16 :: MonadASM m => Word16 -> m ()
fac1ToInt16 location = do
    jsr romFACINX
    sty location
    sta (location + 1)

-- | Calculate f1 = f1 / f2: +div2
div2 :: MonadASM m => Word16 -> Word16 -> m ()
div2 f1 f2 = do
    lda # lo f2
    ldy # hi f2
    jsr romMOVFM
    lda # lo f1
    ldy # hi f1
    jsr romFDIV
    ldx # lo f1
    ldy # hi f1
    jsr romMOVMF

-- | Multiply dt to FAC1 by shifting the exponent: +multiply_dt_to_fac1
multiplyDtToFac1 :: MonadASM m => m ()
multiplyDtToFac1 = do
    sec
    lda facExp
    sbc # dtShift
    sta facExp
    clc

-- | Save int(grad/8) to target: +save_int_gradient_to
saveIntGradientTo :: MonadASM m => Word16 -> m ()
saveIntGradientTo target = do
    -- save int(grad/8) to target
    jsr romMOVFA
    clc
    lda facExp
    sbc # 3
    sta facExp
    fac1ToInt16 target
    jsr romMOVEF

-- | 16-bit left shift: +lshift_16bit
lshift16 :: (MonadASM m, Operand a, Operand b) => a -> b -> m ()
lshift16 hb lb = do
    asl lb
    rol hb

-- | 16-bit right shift: +rshift_16bit
rshift16 :: (MonadASM m, Operand a, Operand b) => a -> b -> m ()
rshift16 hb lb = do
    lsr hb
    ror lb

-- | Debug blit: +dbgblt
dbgblt :: MonadASM m => Word16 -> Word8 -> m ()
dbgblt addr val = do
    -- usage:
    -- dbgblt 0x200 0xFF   draws a straight line in the first row
    lda # val
    sta (addr + 0x2000)

-- | Set key press timer for debouncing: +set_key_press_timer
setKeyPressTimer :: MonadASM m => Var8 -> m ()
setKeyPressTimer kpt = do
    lda # 10  -- number of ISR invocations to wait between key press checks
    sta kpt

-- | Drawloop macro (plot y = FP_A * x + FP_B): +drawloop
drawloop :: MonadASM m => Label -> Label -> Ptr -> m ()
drawloop fastMultLbl blitXyLbl zfb = do
    -- plot y = FP_A / FP_TEMP * x + FP_C / FP_TEMP
    setIntParam fpXcur 0
    setIntParam fpB 40
    setIntParam fpA 3
    setIntParam fpTemp 2
    div2 fpA fpTemp
    -- set sign of FP_A
    -- lda #$ff
    -- sta $66
    -- storeFac fpA
    setIntParam fpC 5
    setIntParam fpTemp 10
    div2 fpC fpTemp
    loopStart <- label
    memToFac1 fpA
    fpMul fpXcur fastMultLbl
    fpAdd fpB
    storeFac fpYcur
    fac1ToInt16 intY
    -- increase x by step
    memToFac1 fpXcur
    fpAdd fpC
    storeFac fpXcur
    fac1ToInt16 intX
    lda intX
    sta zfb
    lda (intX + 1)
    sta (zfb + 1)
    ldy intY
    jsr blitXyLbl
    lda intX
    cmp # 100
    bcc loopStart

-- | Convert a 5-byte CBM float (exponent + 4 mantissa bytes) to a Haskell Double.
cbmFloatToDouble :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Double
cbmFloatToDouble 0 _ _ _ _ = 0.0
cbmFloatToDouble e m0 m1 m2 m3 =
    let sign = if testBit m0 7 then negate else id
        mantissa = fromIntegral (m0 .|. 0x80) `shiftL` 24
               .|. fromIntegral m1 `shiftL` 16
               .|. fromIntegral m2 `shiftL` 8
               .|. fromIntegral m3 :: Integer
    in  sign $ encodeFloat mantissa (fromIntegral e - 160)

-- | Read a 5-byte CBM float from emulator memory at a given address.
readCBMFloat :: Word16 -> CPUState -> Double
readCBMFloat addr s = cbmFloatToDouble
    (rd addr) (rd (addr+1)) (rd (addr+2)) (rd (addr+3)) (rd (addr+4))
  where rd a = view (memAt a) s

-- | Right-pad a string to a given width.
padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

-- =========================================================================
-- Program
-- =========================================================================

program :: (MonadASM m, MonadZPAlloc m) => m Word16
program = mdo

    -- =====================================================================
    -- Zero-page allocations
    -- =====================================================================
    keyPressTimer  <- allocVar8
    sndTurnSoundOn <- allocVar8
    rngStateLo     <- allocVar8
    rngStateHi     <- allocVar8
    sndTurnedOff   <- allocVar8
    foo1           <- allocVar8
    foo2           <- allocVar8
    freqLoV1Buf    <- allocVar8
    freqHiV1Buf    <- allocVar8
    playCounter    <- allocVar8
    accu           <- allocVar16  -- ACCU+0/ACCU+1 in mult_subroutine
    p0             <- allocPtr    -- P0 ($DE/$DF in original)
    zpFB           <- allocPtr    -- $FB/$FC in original
    zpFD           <- allocPtr    -- $FD/$FE in original

    -- =====================================================================
    -- BASIC SYS stub: SYS 2061
    -- =====================================================================
    -- *=$0801
    annotate "basicSysStub" $
        byte [0x0B, 0x08, 0x01, 0x00, 0x9E, 0x32, 0x30, 0x36, 0x31, 0x00, 0x00, 0x00]

    -- =====================================================================
    -- Start: * = $080d (=2061)
    -- =====================================================================

    annotate "initColors" $ do
        -- SetBorderColor 0
        lda # 0
        sta vicBorderColor
        sta vicBackgroundColor0   -- infill color to black as well

        lda # 0x02
        sta screenRAM             -- draw 'B'

    annotate "initInterrupts" $ do
        sei                       -- disable interrupts

        -- setup sound
        jsr initSid

        lda # lo (toAddr vicRstIrq)
        sta sysIRQVec
        lda # hi (toAddr vicRstIrq)
        sta (sysIRQVec + 1)      -- fill interrupt table entry for VIC-II RST interrupt
        -- VIC-II can generate interrupts, these have to be enabled
        -- and, once on occurs, a the bit in the interrupt latch
        -- register ($d019) needs to be cleared.
        --
        -- $d01a is the interrupt enable register - a bit in the
        -- first 4 bits will enable one of the 4 interrupts.
        --
        -- here we will enable the 'reached certain raster line' (RST)
        -- interrupt. The raster line is stored in $d012 and $d011.
        asl vicInterruptStatus
        lda # 0x7B
        sta cia1InterruptControl
        lda # 0x81
        sta vicInterruptEnable    -- write to VIC-II interrupt register
        lda # 0x1B
        sta vicControlY
        lda # 0x80
        sta vicRasterLine
        cli                       -- enable interrupts

    annotate "initBitmapMode" $ do
        -- enable 'high-res' bitmap mode; this gives us 320x200 pixel (=64000)
        -- in graphics memory but only 40x25 (=1000) bytes for color.
        lda vicControlY           -- set BMM=1
        ora # 0b00100000
        sta vicControlY
        lda vicControlX           -- unset MCM
        and_ # 0b11101111
        sta vicControlX

        lda vicMemorySetup
        ora # 0b00001000
        sta vicMemorySetup        -- move graphics to $2000 instead of $1000

    -- colors are defined for 8x8 pixels at once, upper nibble for 'on' pixels.
    -- for simplicity we'll fill all 40x25 byte with white for on-pixels and
    -- black for off-pixels.
    annotate "colorFill" $ do
        lda # 0b01010000
        for_x 0 $ do
            sta (screenRAM, X)
            sta (screenRAM + 0x0100, X)
            sta (screenRAM + 0x0200, X)
            sta (screenRAM + 0x0300, X)

    -- overwrite all pixels with 0 to blank the screen
    --
    -- there are 64000 pixels, 1 bit for each -> 8000 byte for the whole screen.
    -- therefore we have 8000/256 = 31.25 pages to fill, starting at $2000.
    -- $2000, $2100, ...
    --
    -- since we only have 16 bit registers we can use zero-page adressing:
    -- we write $2000 (high byte $20 and low byte $00 respectively) to
    -- memory location $00fc and $00fb. Then we can do something like
    -- sta ($fb), y to set ($2000 + y) to the content of register A.
    annotate "clearScreen" $ do
        lda # 0x00
        sta zpFB
        lda # 0x20
        sta (zpFB + 1)
        lda # 0x00
        for_x 32 $ do
            for_y 0 $ do
                sta (zpFB ! Y)
            inc (zpFB + 1)


    -- 320x200 resolution, 40x25 bytes, therefore 256/40=6.4 rows per page
    --
    -- 0b0 0b1 0b2 0b3 ... 0b7     8b0 8b1 8b2 ... 8b7
    -- 1b0 1b1 1b2 ...             9b0 9b1 ...
    -- 2b0 ... ...                 Ab0 ...
    -- 3b0                         Bb0
    -- 4b0                         Cb0
    -- 5b0                         Db0
    -- 6b0                         Eb0
    -- 7b0                         Fb0
    --
    -- when base addr. = $2000, then 0b0 is bit 0 at $2000, 0b1 is bit 1 at $2000.

    -- =====================================================================
    -- OR/AND mask LUT init (generated from Haskell loop)
    -- =====================================================================
    annotate "initMaskLuts" $
        forM_ [0..7] $ \i -> do
            let orMask = 1 `shiftL` i :: Word8
            lda # orMask
            sta (screenMaskOr0 + fromIntegral i)
            lda # complement orMask
            sta (screenMaskAnd0 + fromIntegral i)


    jsr rngSeed

    annotate "initState" $ do
        lda # 0
        sta keyPressTimer
        sta sndTurnSoundOn

    -- =====================================================================
    -- Float initialization
    -- =====================================================================
    annotate "initFloats" $ mdo
        setIntParam fpXcur 2
        setIntParam fpYcur 1
        setIntParam fpZcur 1

        setIntParam fpA 10
        setIntParam fpB 28
        setIntParam fpC 8

        setIntParam fpScaleY 25
        setIntParam fpOffsetX 160
        setIntParam fp10 10

        -- initialize FP_C = 8/3
        setIntParam fpTemp 3
        -- when doing fast mult we're going to sink into the first attractor
        -- due to error accumulation, so we need to shift C just a tiny bit
        -- so that we don't :)
        lda useFastMult
        beq skipFastMultAdjust
        setIntParam fpTemp2 3
        div2 fpTemp2 fp10
        memToFac1 fpTemp
        fpAdd fpTemp2
        storeFac fpTemp
        skipFastMultAdjust <- label
        div2 fpC fpTemp

        lda # 0
        sta useFastMult

        -- Comment the following jump to reach the fast mult test drawing code.
        jmp mainLoop



    -- =====================================================================
    -- TEST CODE
    -- =====================================================================
    annotate "fastMultTests" $ do
        -- testing fast float multiplication
        --
        -- FP_A = 3.1415
        -- ['0x82', '0x49', '0xe', '0x56', '0x0']
        lda # 0x82
        sta fpA
        lda # 0x49
        sta (fpA + 1)
        lda # 0x0E
        sta (fpA + 2)
        lda # 0x56
        sta (fpA + 3)
        lda # 0x00
        sta (fpA + 4)
        memToFac1 fpA
        -- FP_B = -10
        -- ['0x84', '0xa0', '0x0', '0x0', '0x0']
        lda # 0x84
        sta fpB
        lda # 0xA0
        sta (fpB + 1)
        lda # 0x00
        sta (fpB + 2)
        lda # 0x00
        sta (fpB + 3)
        lda # 0x00
        sta (fpB + 4)
        fpMul fpB fastMult
        storeFac fpC

        -- code for multiplying 3.1415 and 100
        -- inspect $c48a for result
        -- expected for approx mult: 290.11199951171875
        lda # 0x82
        sta (0xC480 :: Word16)
        lda # 0x49
        sta (0xC481 :: Word16)
        lda # 0x0E
        sta (0xC482 :: Word16)
        lda # 0x56
        sta (0xC483 :: Word16)
        lda # 0x00
        sta (0xC484 :: Word16)
        memToFac1 0xC480
        lda # 0x87
        sta (0xC485 :: Word16)
        lda # 0x48
        sta (0xC486 :: Word16)
        lda # 0x00
        sta (0xC487 :: Word16)
        lda # 0x00
        sta (0xC488 :: Word16)
        lda # 0x00
        sta (0xC489 :: Word16)
        fpMul 0xC485 fastMult
        storeFac 0xC48A

        -- code for multiplying 3.1415 and 1000
        -- inspect $c48a for result
        -- expected for approx mult: 3120.89599609375
        lda # 0x82
        sta (0xC480 :: Word16)
        lda # 0x49
        sta (0xC481 :: Word16)
        lda # 0x0E
        sta (0xC482 :: Word16)
        lda # 0x56
        sta (0xC483 :: Word16)
        lda # 0x00
        sta (0xC484 :: Word16)
        memToFac1 0xC480
        lda # 0x8A
        sta (0xC485 :: Word16)
        lda # 0x7A
        sta (0xC486 :: Word16)
        lda # 0x00
        sta (0xC487 :: Word16)
        lda # 0x00
        sta (0xC488 :: Word16)
        lda # 0x00
        sta (0xC489 :: Word16)
        fpMul 0xC485 fastMult
        storeFac 0xC48A

        -- drawloop test code
        lda # 0
        sta useFastMult
        drawloop fastMult blitXy zpFB
        lda # 1
        sta useFastMult
        drawloop fastMult blitXy zpFB

        jmp hang

    -- =====================================================================
    -- End of fast mult test drawing code
    -- =====================================================================



    -- =====================================================================
    -- Main loop
    -- =====================================================================
    mainLoop <- label
    afterXyzStep <- annotate "mainLoop" $ do
        lda # (0x20 - 5)
        sta (0xFA :: Word8)
        drawLoop <- label
        jsr xyzStep
        afterStep <- label
        lda intX
        sta zpFB
        lda (intX + 1)
        sta (zpFB + 1)
        ldy intY

        jsr blitXy

        jsr rngNext
        jsr clearRngPixel
        jsr rngNext
        jsr clearRngPixel

        dec (0xFA :: Word8)
        jmp drawLoop
        pure afterStep



    hang <- label
    annotate "hang" $ jmp hang


    -- =====================================================================
    -- fast_mult subroutine
    -- =====================================================================
    fastMult <- label
    annotate "fastMult" $ mdo
        -- similar interface to FMULT:
        -- Multiplies a number from RAM and FAC (clobbers ARG, A=Addr.LB, Y=Addr.HB)
        pha
        -- load the 2nd number to ARG using CONUPK
        jsr romCONUPK

        -- If one of the exponents is zero, we can return early as the
        -- result will be zero.
        lda facExp
        beq fmZeroExp
        lda argExp
        bne fmNotZero
        fmZeroExp <- label
        -- Store zero exponent and keep mantissa intact as it will be
        -- ignored with a zero exponent.
        sta facExp
        pla
        clc
        rts
        fmNotZero <- label

        -- Multi-byte addition of the mantissas of ARG and FAC1 from least
        -- to most significant to utilize the carry bit.
        clc
        lda facMant3
        adc argMant3
        sta facMant3

        lda facMant2
        adc argMant2
        sta facMant2

        lda facMant1
        adc argMant1
        sta facMant1

        -- Both bytes will have the MSB set for normalization, so it will
        -- always overflow, no matter if that is 'necessary' or not. Therefore
        -- we will determine the overflow by masking the MSB and checking bit 7.
        lda facMant0
        and_ # 0x7F
        sta zpFB
        lda argMant0
        and_ # 0x7F
        adc zpFB
        -- if the MSB is set (i.e. not "positive") we know that we overflowed
        -- to the 2**-1 (0.5) position, i.e. we're adding another 0.5 (we're
        -- always adding 0.5, so that makes 0.5+0.5=1). Therefore we need to
        -- set bit 0 of the exponent (2**0=1) in that case (i.e., set the carry).
        clc
        bpl fmNoMantOvf
        sec
        fmNoMantOvf <- label
        -- make sure that the MSB mantissa bit is always 1 (normalization)
        ora # 0x80
        sta facMant0

        -- This adds the two exponents, they'll most likely overflow.
        -- we'll correct this by subtracting 128+ from the exponent below.
        -- We're also adding the carry from the mantissa if there's one
        -- implicitly (since the carry flag will be set if needed).
        lda facExp
        adc argExp
        sta facExp

        -- Check if the addition carried over to the high bit. If it did not carry
        -- over, we suspect that the exponent is < 128 which means that if we
        -- subtract 128, we're underflowing the exponent value. In that case we
        -- should simply set the exponent to 0 since it is the smallest value
        -- we can use in a case of an underflow (2**(130 - 128)
        bcs fmNoUnderflow

        -- to make sure, let's check if the 8 bit portion is also < 128, then
        -- we're sure there's an underflow.
        lda # 128
        cmp facExp          -- C = A >= M = 128 >= M
        bcc fmNoUnderflow   -- no underflow, exponent is >= 129
        -- underflow
        lda # 0
        sta facExp
        jmp fmDone
        fmNoUnderflow <- label
        -- subtract 129 from the added exponents
        lda facExp
        sec
        sbc # 129
        sta facExp
        fmDone <- label

        -- sign bit handling; XOR sign byte
        lda facSign
        eor argSign
        sta facSign

        lda # 0
        sta facOverflow

        pla
        clc
        rts


    -- =====================================================================
    -- xyz_step subroutine (Lorenz attractor step + Y/magnitude computation)
    -- =====================================================================
    xyzStep <- label
    annotate "xyzStep" $ mdo
        -- X_new = a * (Y_cur - X_cur)
        -- X_cur = X_cur + X_new * dt
        --
        -- 1. Y_cur - X_cur
        memToFac1 fpXcur
        lda # lo fpYcur
        ldy # hi fpYcur
        jsr romFSUB
        -- 2. a * FAC1
        lda # lo fpA
        ldy # hi fpA
        -- jsr romFMULT
        jsr fastMult
        saveIntGradientTo intXdt
        -- 3. FAC1 * dt
        multiplyDtToFac1
        -- 4. FAC1 + X_cur
        lda # lo fpXcur
        ldy # hi fpXcur
        jsr romFADD
        -- 4. X_cur = FAC1
        ldx # lo fpXcur
        ldy # hi fpXcur
        jsr romMOVMF



        -- store int(fp_x) as X coordinate
        --
        -- we're multiplying X_CUR by 8 but
        -- we don't really multiply, we just
        -- add 3 to the float's exponent.
        --
        -- original code:
        -- setIntParam fpScaleX 8
        -- [...]
        -- lda # lo fpScaleX
        -- ldy # hi fpScaleX
        -- jsr romFMULT
        clc
        lda # 3
        adc facExp
        sta facExp
        lda # lo fpOffsetX
        ldy # hi fpOffsetX
        jsr romFADD
        fac1ToInt16 intX


        -- Y_new = X_cur * (b - Z_cur) - Y_cur
        -- Y_cur = Y_cur + Y_new * dt

        -- (b - Z_cur)
        memToFac1 fpZcur
        fpSub fpB
        -- FAC1 * X_cur
        fpMul fpXcur fastMult
        -- FAC1 = ARG - FAC1
        --  ARG = FAC1 (= X_cur * (b - Z_cur))
        --  FAC1 = Y_cur
        jsr romMOVFA              -- ARG = FAC1
        memToFac1 fpYcur
        jsr romFSUBT
        saveIntGradientTo intYdt
        -- FAC1 * dt
        multiplyDtToFac1
        -- FAC1 + Y_cur
        fpAdd fpYcur
        -- Y_cur = FAC1
        storeFac fpYcur




        -- Z_new = X_cur * Y_cur - c * Z_cur
        -- Z_cur = Z_cur + Z_new * dt
        --
        -- 1. temp=(c * Z_cur)
        memToFac1 fpC
        fpMul fpZcur fastMult
        storeFac fpTemp
        -- 2. (X_cur * Y_cur)
        memToFac1 fpXcur
        fpMul fpYcur fastMult
        -- 3. FAC1 - temp
        jsr romMOVFA
        memToFac1 fpTemp
        jsr romFSUBT
        saveIntGradientTo intZdt
        -- 4. FAC1 * dt
        multiplyDtToFac1
        -- 5. FAC1 + Z_cur
        fpAdd fpZcur
        -- 6. Z_cur = FAC1
        storeFac fpZcur



        -- compute Y addr.
        -- u = (y + z * 10)
        -- y_px = int((u * 25)) >> 6
        --
        -- store int(y + z*10) as Y coordinate
        -- note that FAC1 contains Z_CUR at this point in time.
        fpMul fp10 fastMult
        fpAdd fpYcur
        fpMul fpScaleY fastMult
        -- normally we'd use fac1ToInt16 but it assumes signed
        -- integer (which we don't expect) and therefore values >32k
        -- will break. instead we'll use QINT and take the lowest
        -- two bytes.
        jsr romQINT
        lda facMant2              -- HB of 16 bit int
        ldy facMant3              -- LB of 16 bit int
        sta (intY + 1)
        sty intY
        rshift16 (intY + 1) intY
        rshift16 (intY + 1) intY
        rshift16 (intY + 1) intY
        rshift16 (intY + 1) intY
        rshift16 (intY + 1) intY
        rshift16 (intY + 1) intY

        -- from here on we will only use INT_Y since the possible Y
        -- range is from 0 to 200 anyway.
        -- we need to invert it so the image is not flipped, though.
        clc
        lda # 200
        sbc intY
        sta intY

        -- compute l2 norm L2(int(dx), int(dy)+2int(dz)) to get the current
        -- curve's magnitude. note that we only scale dz by 2 as simulations
        -- deemed this sufficient and safes us some computation.
        --
        -- * we use an L2 approximation (max(x,y) + 1/2*min(x,y)) to avoid
        --   sqrt and squaring :)
        -- * we scale both dx and dy+2dz by 1/4 to get a good value range
        --   for our purporses (math. correctness to hell)
        --
        --
        lda intYdt
        clc
        adc intZdt
        adc intZdt
        sta intM              -- INT_M = A = dy + 2dz
        cmp intXdt

        annotate "l2Magnitude" $
            if_cs
                (do -- x is max, y is min
                    lda intM
                    lsr_a
                    lsr_a             -- scale dy+2dz value by 4 (we do x/4 later)
                    lsr_a             -- scale INT_M by 2 since it contains the minimum
                    sta intM
                    lda intXdt
                    lsr_a
                    lsr_a
                    clc
                    adc intM          -- INT_M = dx/4 (max) + ((dy + 2dz)/4)/2
                    sta intM)
                (do -- y is max, x is min
                    lda intM
                    lsr_a
                    lsr_a             -- INT_M = (dy+2dz)/4 (max)
                    sta intM
                    lda intXdt
                    lsr_a
                    lsr_a             -- scale dx by 4
                    lsr_a             -- scale dx by 2 because its the minimum
                    clc
                    adc intM
                    sta intM)

        rts


    -- =====================================================================
    -- RNG (linear congruential generator)
    -- X(n+1) = (a * X(n) + c) mod m
    -- a = 5, c = 1, m = 65536 (2**16)
    -- =====================================================================
    rngSeed <- label
    annotate "rngSeed" $ do
        -- read current horizontal scanline position from VIC-II
        lda vicRasterLine
        sta rngStateLo
        lda # 0x00
        sta rngStateHi
        rts

    rngNext <- label
    annotate "rngNext" $ do
        -- X(n+1) = (5 * X(n) + 1) mod 65536
        -- compute 5 * X(n) as (4 + 1) * X(n)
        lda rngStateLo
        sta zpFB
        lda rngStateHi
        sta (zpFB + 1)

        -- X(n) << 2
        lshift16 (zpFB + 1) zpFB
        lshift16 (zpFB + 1) zpFB

        -- add X(n)
        clc
        lda rngStateLo
        adc zpFB
        sta rngStateLo
        lda rngStateHi
        adc (zpFB + 1)
        sta rngStateHi

        -- add 1
        clc
        lda rngStateLo
        adc # 1
        sta rngStateLo
        lda rngStateHi
        adc # 0
        sta rngStateHi

        rts


    -- =====================================================================
    -- blit_xy subroutine
    -- =====================================================================
    blitXy <- label
    annotate "blitXy" $ fitsIn 160 $ do
        -- parameters: x (16 bit), y (8 bit)
        -- 0 <= x < 320, 0 <= y < 200
        --
        -- assume x is in zpFB (lo/hi)
        -- assume y is in Y
        --
        -- clobbers SCREEN_ADDR global, zpFB and zpFD.

        -- this is a quest to resolve x/y coordinates into an
        -- screen buffer address. we're assuming 0x2000 as base
        -- address.
        --
        -- since we have 40x25 byte (8 pixel each, giving 320x200 pixel)
        -- we have a global adressing (byte-level) and a local adressing
        -- (bit-level).

        -- intitialize addr. variable to 0x2000
        lda # 0x00
        sta scrAddr
        lda # 0x20
        sta (scrAddr + 1)

        -- compute pixel mask to OR on the region; this will set the pixel bit
        -- in the byte for which we're currently computing the address of.
        -- _screen_mask
        lda zpFB
        and_ # 7
        eor # 7
        tax

        -- we round the X offset to a power of 8 since we have
        -- 8 pixel for each adressable byte (pixels are bits, remember).
        --
        -- addr = addr + (x & 0xF8)
        --                ^^^^^^^^ -> x.LB = (x.LB & 8)
        -- _x_shift
        clc
        lda zpFB
        and_ # 0xF8
        adc scrAddr
        sta scrAddr
        lda (zpFB + 1)
        adc (scrAddr + 1)
        sta (scrAddr + 1)

        -- _y_shift_global
        --    yoff_row = (y >> 3) * 40 * 8
        --    yoff_row = (y & 0xF8) * 40
        --    u = y & 0xF8
        --    y_off_row = u * 40
        --    y_off_row = u * ((1 << 5) + (1 << 3))
        --    y_off_row = (u << 5) + (u << 3)
        --
        -- clear high byte of (zpFB) and (zpFD)
        lda # 0
        sta (zpFB + 1)
        sta (zpFD + 1)
        -- init low bytes to y * 0xF8 (u = y & 0xF8)
        tya
        and_ # 0xF8
        sta zpFB
        sta zpFD
        -- y1 = u << 5
        lshift16 (zpFB + 1) zpFB
        lshift16 (zpFB + 1) zpFB
        lshift16 (zpFB + 1) zpFB
        lshift16 (zpFB + 1) zpFB
        lshift16 (zpFB + 1) zpFB
        -- y2 = u << 3
        lshift16 (zpFD + 1) zpFD
        lshift16 (zpFD + 1) zpFD
        lshift16 (zpFD + 1) zpFD
        -- y_off_row = y1 + y2
        clc
        lda zpFD
        adc zpFB
        sta zpFB
        lda (zpFD + 1)
        adc (zpFB + 1)
        sta (zpFB + 1)

        -- add y_off_row to screen addr.
        clc
        lda scrAddr
        adc zpFB
        sta scrAddr
        lda (scrAddr + 1)
        adc (zpFB + 1)
        sta (scrAddr + 1)

        -- add yoff_local to screen_addr
        -- _y_shift_local
        clc
        tya
        and_ # 7
        adc scrAddr
        sta scrAddr
        lda # 0
        adc (scrAddr + 1)
        sta (scrAddr + 1)

        -- load addr., mask pattern, store again
        lda scrAddr
        sta zpFB
        lda (scrAddr + 1)
        sta (zpFB + 1)
        ldy # 0
        lda (zpFB ! Y)
        ora (screenMaskOr0, X)
        sta (zpFB ! Y)

        rts



    -- =====================================================================
    -- clear_rng_pixel subroutine
    -- =====================================================================
    clearRngPixel <- label
    annotate "clearRngPixel" $ fitsIn 64 $ do
        -- parameters: rngStateLo, rngStateHi as linear screen memory offset
        --
        -- clobbers SCREEN_ADDR global, zpFB and zpFD.

        -- load rng state into screen addr
        lda rngStateLo
        sta scrAddr
        lda rngStateHi
        sta (scrAddr + 1)
        -- divide by 8 to convert from pixel offset to byte offset
        lsr (scrAddr + 1)
        ror scrAddr
        lsr (scrAddr + 1)
        ror scrAddr
        lsr (scrAddr + 1)
        ror scrAddr
        -- add baseline screen address 0x2000
        lda # 0x20
        adc (scrAddr + 1)
        sta (scrAddr + 1)

        lda rngStateLo
        and_ # 7
        tax

        -- load addr., mask pattern, store again
        lda scrAddr
        sta zpFB
        lda (scrAddr + 1)
        sta (zpFB + 1)

        ldy # 0
        lda (zpFB ! Y)
        and_ (screenMaskAnd0, X)
        sta (zpFB ! Y)

        rts



    -- =====================================================================
    -- Data: initial values for SID initialization
    -- =====================================================================

    -- initial pulse wave duty cycles for each voice
    initValuesPulse <- label
    byte [0x08, 0x03, 0x03]

    -- initial wave form for each voice
    --
    -- bit  desc.
    -- 7    noise
    -- 6    pulse
    -- 5    sawtooth
    -- 4    triangle
    -- 3    test
    -- 2    ring modulation with voice N (1:3, 2:1, 3:1)
    -- 1    sync with voice N (1:3, 2:1, 3:1)
    -- 0    gate
    initValuesWave <- label
    byte [0x08, 0x08, 0x08]

    -- global filter and main volume config for
    -- register $d416, $d417 and $d418
    --
    -- $d416: filter cutoff freq high byte (bits $d415:{3..0} are the low byte)
    -- $d417: filter resonance and routing config
    --   7..4: filter resonance
    --      3: external input into filter
    --      2: voice 3 into filter?
    --      1: voice 2 into filter?
    --      0: voice 1 into filter?
    -- $d418: filter mode and main volume control
    --      7: mute voice 3
    --      6: high pass
    --      5: band pass
    --      4: low pass
    --   3..0: main volume
    initValuesSid <- label
    byte [0x00, 0xF4, 0x1F]


    -- =====================================================================
    -- init_sid subroutine
    -- =====================================================================
    -- this code initializes the SID memory starting at $d400.
    --
    -- SID has 3 configurable voices which are initialized here.
    initSid <- label
    annotate "initSid" $ do
        lda # 1
        sta sndTurnedOff

        ldy # 0x18
        lda # 0x00
        initSidLoop1 <- label
        sta (sidIOBase, Y)
        dey
        bpl initSidLoop1      -- clear the SID memory with zeroes

        -- populate voice settings with sensible values
        ldy # 0x0E             -- y = voice offset in SID memory
        ldx # 0x02             -- x = voice index
        initSidLoop2 <- label
        lda (initValuesPulse, X)
        sta (sidV1PulseWidthHi, Y)

        lda (initValuesWave, X)
        sta (sidV1Control, Y)

        lda # 0x00
        sta (sidV1AttackDecay, Y)

        lda (initValuesSid, X)
        sta (sidFilterCutoffHi, X)  -- set filter cutoff, resonance and mode / main volume
                                    -- abuses x for writing several bytes but does not
                                    -- depend on a voice (is a global setting)
        tya
        sec
        sbc # 0x07
        tay
        dex
        bpl initSidLoop2


        -- TEST CODE PLS REMOVE THX

        lda # 0b01000001
        sta sidV1Control

        ldy # 29              -- D + 35c
        lda (freqlo, Y)
        sta sidV1FreqLo
        lda (freqhi, Y)
        sta sidV1FreqHi

        -- wav duty is 12 bit =)
        -- fun fact: half of 2^12 is 2^11!
        lda # 0xFF
        sta sidV1PulseWidthLo
        lda # 7
        sta sidV1PulseWidthHi

        lda # 0x00
        sta sidV1AttackDecay
        lda # 0xF0
        sta sidV1SustainRelease

        -- setup voice 2
        lda # 0b01000001
        sta sidV2Control

        lda # 0xFF
        sta sidV2PulseWidthLo
        lda # 7
        sta sidV2PulseWidthHi

        lda # 0x00
        sta sidV2AttackDecay
        lda # 0xF0
        sta sidV2SustainRelease

        -- setup voice 3
        lda # 0b01000001
        sta sidV3Control

        lda # 0xFF
        sta sidV3PulseWidthLo
        lda # 7
        sta sidV3PulseWidthHi

        lda # 0x00
        sta sidV3AttackDecay
        lda # 0xF0
        sta sidV3SustainRelease

        lda # 0
        sta foo1
        lda # 0
        sta foo2


        lda sidFilterResonanceRouting
        ora # 0b11110001
        sta sidFilterResonanceRouting



        rts


    -- =====================================================================
    -- reset_screen subroutine
    -- =====================================================================
    -- sub-routine to clear the screen and reset the state of the drawing
    resetScreen <- label
    annotate "resetScreen" $ do
        setIntParam fpXcur 2
        setIntParam fpYcur 1
        setIntParam fpZcur 1

        lda # 0x00
        sta zpFB
        lda # 0x20
        sta (zpFB + 1)
        lda # 0
        for_x 32 $ do
            for_y 0 $ do
                sta (zpFB ! Y)
            inc (zpFB + 1)

        rts


    -- =====================================================================
    -- handle_key_presses subroutine
    -- =====================================================================
    --
    --          CIA 1 Port B ($DC01)    Joy 2
    --          PB7   PB6   PB5   PB4   PB3   PB2   PB1   PB0
    --   CIA1
    --   Port A
    --   ($DC00)
    --   PA7  STOP    Q    C=   SPACE   2    CTRL   <-     1
    --   PA6   /      ^     =  RSHIFT  HOME   ;      *     £
    --   PA5   ,      @     :    .      -      L     P      +
    --   PA4   N      O     K    M      0      J     I      9    Fire
    --   PA3   V      U     H    B      8      G     Y      7    Right
    --   PA2   X      T     F    C      6      D     R      5    Left
    --   PA1  LSHIFT  E     S    Z      4      A     W      3    Down
    --   PA0  CRSRDN  F5    F3   F1     F7   CRSRRT RETURN DELETE Up
    --   Joy 1                          Fire  Right  Left   Down   Up
    --
    --   https://www.c64-wiki.com/wiki/Keyboard

    handleKeyPresses <- label
    annotate "handleKeyPresses" $ mdo
        -- we wait a fixed amount of ISR invocations between key presses
        -- to debounce.
        lda keyPressTimer
        beq hkpNoWait

        dec keyPressTimer
        rts

        hkpNoWait <- label

        -- default is to turn sound off after reading the keys.
        lda # 0
        sta sndTurnSoundOn

        -- 'Q' key press handler
        --
        -- activate sequence 1 for left
        lda # 0b01111111
        sta cia1DataPortA
        lda cia1DataPortB
        and_ # 0b01000000
        when_eq $ do
            setKeyPressTimer keyPressTimer
            lda # 1
            sta sndTurnSoundOn

        -- 'A' key press handler
        --
        -- activate sequence 2 for left
        lda # 0b11111101
        sta cia1DataPortA
        lda cia1DataPortB
        and_ # 0b00000100
        when_eq $ do
            setKeyPressTimer keyPressTimer
            lda # 1
            sta sndTurnSoundOn

        -- 'Z' key press handler
        lda # 0b11111101
        sta cia1DataPortA
        lda cia1DataPortB
        and_ # 0b00010000
        when_eq $ do
            setKeyPressTimer keyPressTimer
            lda # 1
            sta sndTurnSoundOn

        -- we now know if we need to toggle sound on or not.
        -- let's do that now!
        --
        -- if SND_TURN_SOUND_ON and SND_TURNED_OFF:
        --   SND_TURNED_OFF = False
        --
        -- if not SND_TURN_SOUND_ON and not SND_TURNED_OFF:
        --   SND_TURNED_OFF = True
        dbgblt 0x200 0x00
        lda sndTurnSoundOn
        if_ne
            (do lda sndTurnedOff
                when_ne $ do dbgblt 0x200 0xFF; lda # 0; sta sndTurnedOff)
            (do lda sndTurnedOff
                when_eq $ do dbgblt 0x200 1; lda # 1; sta sndTurnedOff)

        -- 'R' key press handler
        --
        -- we hijack this interrupt for checking if the user pressed R
        -- to reset the program. lazyness :)
        lda # 0b11111011
        sta cia1DataPortA
        lda cia1DataPortB
        and_ # 0b00000010
        when_eq $ do
            setKeyPressTimer keyPressTimer
            jsr resetScreen

        -- 'F' key press handler
        lda # 0b11111011
        sta cia1DataPortA
        lda cia1DataPortB
        and_ # 0b00100000
        when_eq $ do
            setKeyPressTimer keyPressTimer
            -- Toggle fast multiplication when F is pressed
            lda useFastMult
            if_ne (do lda # 0; sta useFastMult)
                  (do lda # 1; sta useFastMult)

        rts


    -- =====================================================================
    -- vic_rst_irq interrupt handler
    -- =====================================================================
    vicRstIrq <- label
    annotate "vicRstIrq" $ fitsIn 128 $ mdo
        asl vicInterruptStatus     -- clear latch bit of RST interrupt
        -- SetBorderColor 2
        lda # 2
        sta vicBorderColor

        -- call musicplay while saving the raster line before the call
        -- and subtracting the current raster line after the call to get
        -- the elapsed time (in elapsed raster lines) to plot it on the
        -- screen. as an additional effect we also change the background
        -- color for the 'processing' lines.
        lda vicRasterLine
        sta timer
        -- jsr playSounds
        jsr handleKeyPresses
        jsr musicplay
        lda vicRasterLine
        sec
        sbc timer
        clc
        adc # 0x30
        cmp (screenRAM + 1)        -- read first 'character' of screen memory
        bcc notBigger
        sta (screenRAM + 1)
        notBigger <- label
        -- SetBorderColor 0
        lda # 0
        sta vicBorderColor
        pla
        tay
        pla
        tax
        pla
        rti                        -- restore Y, X, A and return from interrupt

    timer <- label
    byte [0]




    -- =====================================================================
    -- Music play system
    -- =====================================================================
    musicplay <- label
    annotate "musicplay" $ jmp play

    -- !set hardrestartcounter=3

    -- hardrestartindex: value to put into wave in hardrestartframes (from right to left)
    annotate "hardrestartindex" $
        byte [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]


    -- initial pulse wave duty cycles for each voice
    annotate "pulseinit" $
        byte [0x08, 0x03, 0x03]

    -- initial wave form for each voice
    --
    -- bit  desc.
    -- 7    noise
    -- 6    pulse
    -- 5    sawtooth
    -- 4    triangle
    -- 3    test
    -- 2    ring modulation with voice N (1:3, 2:1, 3:1)
    -- 1    sync with voice N (1:3, 2:1, 3:1)
    -- 0    gate
    annotate "waveinit" $
        byte [0x08, 0x08, 0x08]


    -- global filter and main volume config for
    -- register $d416, $d417 and $d418
    --
    -- (same definitions as above, repeated in the play zone of the original)


    -- =====================================================================
    -- mult_subroutine
    -- =====================================================================
    multSubroutine <- label
    annotate "multSubroutine" $ mdo
        -- lda #<FREQ_LO_VOICE1
        -- lda #<FILTER_CUTOFF_LO
        lda # lo testMem
        sta p0
        -- lda #>FREQ_LO_VOICE1
        -- lda #>FILTER_CUTOFF_LO
        lda # hi testMem
        sta (p0 + 1)

        -- Inlined mult_subroutine.asm
        -- P2 = FP_YCUR, P3 = FP_YCUR+2, P4 = FP_YCUR+1
        -- P0 = pointer, ACCU = 16-bit accumulator

        -- .s4:
        lda fpYcur                 -- LDA P2 ; (exp + 0)
        beq msS14                  -- BEQ .s14
        -- .s5:
        sec
        sbc # 0x80
        sta (lo16 accu)            -- STA ACCU + 0
        lda # 0x00
        sbc # 0x00
        sta (hi16 accu)            -- STA ACCU + 1
        lda (lo16 accu)            -- LDA ACCU + 0
        bmi msS6                   -- BMI .s6
        -- .s15:
        cmp # 0x09
        bcc msS6                   -- BCC .s6
        -- .s13:
        lda # 0xFF
        -- .s14:
        msS14 <- label
        ldy # 0x00
        sta (p0 ! Y)              -- STA (P0),y
        iny
        -- .s3:
        msS3 <- label
        sta (p0 ! Y)              -- STA (P0),y
        rts
        -- .s6:
        msS6 <- label
        sec
        lda # 0x00
        sbc (lo16 accu)            -- SBC ACCU + 0
        tax
        lda # 0x00
        sbc (hi16 accu)            -- SBC ACCU + 1
        bmi msS7                   -- BMI .s7
        -- .s12:
        bne msS10                  -- BNE .s10
        -- .s11:
        cpx # 0x08
        bcc msS7                   -- BCC .s7
        -- .s10:
        msS10 <- label
        lda # 0x00
        beq msS14                  -- BEQ .s14
        -- .s7:
        msS7 <- label
        txa
        clc
        adc # 0x08
        tax
        lda (fpYcur + 1)           -- LDA P4 ; (m12 + 1)
        ora # 0x80
        cpx # 0x00
        beq msS9                   -- BEQ .s9
        -- .l8:
        msL8 <- label
        lsr_a
        ror (fpYcur + 2)           -- ROR P3 ; (m12 + 0)
        dex
        bne msL8                   -- BNE .l8
        -- .s9:
        msS9 <- label
        ldy # 0x01
        sta (p0 ! Y)              -- STA (P0),y
        lda (fpYcur + 2)           -- LDA P3 ; (m12 + 0)
        dey
        beq msS3                   -- BEQ .s3


    -- =====================================================================
    -- play subroutine
    -- =====================================================================
    play <- label
    annotate "play" $ mdo

        lda fpYcur
        sta (0x2010 :: Word16)
        lda (fpYcur + 1)
        sta (0x2012 :: Word16)
        lda (fpYcur + 2)
        sta (0x2014 :: Word16)

        jsr multSubroutine

        rshift16 (testMem + 1) testMem
        rshift16 (testMem + 1) testMem
        rshift16 (testMem + 1) testMem
        rshift16 (testMem + 1) testMem
        rshift16 (testMem + 1) testMem
        lda testMem
        and_ # 3
        sta sidFilterCutoffLo
        lda (testMem + 1)
        and_ # 3
        asl_a
        asl_a
        asl_a
        asl_a
        asl_a
        sta p0                     -- use p0 as temp (was $de in original)
        lda testMem
        lsr_a
        lsr_a
        lsr_a
        ora p0
        sta sidFilterCutoffHi


        rts


        -- ===== Dead code: filter cutoff alternative 1 =====
        -- filter cutoff; sounds nice!
        lda sidFilterResonanceRouting
        ora # 0b11110001
        sta sidFilterResonanceRouting

        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0x0F
        asl_a
        asl_a
        asl_a
        asl_a
        sta sidFilterCutoffLo
        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0xF0
        lsr_a
        lsr_a
        lsr_a
        lsr_a
        sta sidFilterCutoffHi
        rts



        rts


        -- ===== Dead code: filter cutoff alternative 2 =====
        -- filter cutoff; sounds nice!
        lda sidFilterResonanceRouting
        ora # 0b11110001
        sta sidFilterResonanceRouting

        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0x0F
        asl_a
        asl_a
        asl_a
        asl_a
        sta sidFilterCutoffLo
        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0xF0
        lsr_a
        lsr_a
        lsr_a
        lsr_a
        sta sidFilterCutoffHi
        rts



        -- ===== Dead code: PWM sweep with attractor =====
        -- PWM sweep with attractor
        -- sounds nice
        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0x0F
        asl_a
        asl_a
        asl_a
        asl_a
        sta sidV1PulseWidthLo
        lda (fpYcur + 1)
        eor # 0b10000000
        and_ # 0xF0
        lsr_a
        lsr_a
        lsr_a
        lsr_a
        sta sidV1PulseWidthHi

        rts


        -- ===== Dead code: direct frequency from attractor =====
        lda (fpYcur + 1)
        eor # 0b10000000
        sta sidV1FreqHi
        lda (fpYcur + 2)
        sta sidV1FreqLo

        lda (fpXcur + 1)
        eor # 0b10000000
        sta sidV2FreqHi
        lda (fpXcur + 2)
        sta sidV2FreqLo

        lda (fpZcur + 1)
        eor # 0b10000000
        sta sidV3FreqHi
        lda (fpZcur + 2)
        sta sidV3FreqLo

        rts


        -- ===== Dead code: arpeggio attempt =====

        ldy # 29
        lda (freqlo, Y)
        sta freqLoV1Buf
        lda (freqhi, Y)
        sta freqHiV1Buf

        -- >>> Counter([n >> 5 for n in range(0, 256)])
        -- Counter({0: 32, 1: 32, 2: 32, 3: 32, 4: 32, 5: 32, 6: 32, 7: 32})
        lda playCounter
        lsr_a
        lsr_a
        lsr_a
        lsr_a
        lsr_a

        tay
        lda (arpeggio, Y)
        clc
        adc # 29
        tay
        sec
        lda (freqlo, Y)
        sbc freqLoV1Buf
        sta foo1
        lda (freqhi, Y)
        sbc freqHiV1Buf
        sta foo2


        ldy # 29
        clc
        lda (freqlo, Y)
        adc foo1
        sta freqLoV1Buf
        lda (freqhi, Y)
        adc foo2
        sta freqHiV1Buf


        lda freqLoV1Buf
        sta sidV1FreqLo
        lda freqHiV1Buf
        sta sidV1FreqHi

        lda # 16
        clc
        adc playCounter
        sta playCounter




        rts

        -- ===== Dead code: vibrato testing =====

        ldy # 29
        lda (freqlo, Y)
        sta freqLoV1Buf
        lda (freqhi, Y)
        sta freqHiV1Buf

        lda playCounter
        cmp # 127
        bcs playNoVibrato

        lda # 29
        clc
        adc # 7
        tay
        sec
        lda (freqlo, Y)
        sbc freqLoV1Buf
        sta foo1
        lda (freqhi, Y)
        sbc freqHiV1Buf
        sta foo2

        ldy # 29
        clc
        lda (freqlo, Y)
        adc foo1
        sta freqLoV1Buf
        lda (freqhi, Y)
        adc foo2
        sta freqHiV1Buf
        playNoVibrato <- label

        lda freqLoV1Buf
        sta sidV1FreqLo
        lda freqHiV1Buf
        sta sidV1FreqHi

        lda # 48
        clc
        adc playCounter
        sta playCounter


        rts

        -- ===== Dead code: frequency + PWM from attractor =====
        lda (fpYcur + 1)
        eor # 0b10000000
        sta sidV1FreqHi
        lda (fpYcur + 2)
        sta sidV1FreqLo
        lda fpYcur
        and_ # 0x0F
        asl_a
        asl_a
        asl_a
        asl_a
        sta sidV1PulseWidthLo
        lda fpYcur
        and_ # 0xF0
        lsr_a
        lsr_a
        lsr_a
        lsr_a
        sta sidV1PulseWidthHi

        rts

        -- ===== Dead code: all 3 voices frequency + PWM =====
        lda (fpXcur + 1)
        eor # 0b10000000
        sta sidV2FreqHi
        lda (fpXcur + 2)
        sta sidV2FreqLo
        lda fpXcur
        sta sidV2PulseWidthLo
        lda # 0
        sta sidV2PulseWidthHi

        lda (fpZcur + 1)
        eor # 0b10000000
        sta sidV3FreqHi
        lda (fpZcur + 2)
        sta sidV3FreqLo
        lda fpZcur
        sta sidV3PulseWidthLo
        lda # 0
        sta sidV3PulseWidthHi

        rts


    -- =====================================================================
    -- Frequency tables (page-aligned for indexed access)
    -- =====================================================================
    alignPage
    freqlo <- label
    annotate "freqlo" $
        byte [ 0x0C, 0x1C, 0x2D, 0x3E, 0x47, 0x66, 0x7B, 0x91
             , 0xA9, 0xC3, 0xDD, 0xFA, 0x18, 0x38, 0x5A, 0x7D
             , 0xA3, 0xCC, 0xF6, 0x23, 0x53, 0x86, 0xBB, 0xF4
             , 0x30, 0x70, 0xB4, 0xFB, 0x47, 0x98, 0xED, 0x47
             , 0xA7, 0x0C, 0x77, 0xE9, 0x61, 0xE1, 0x68, 0xF7
             , 0x8F, 0x30, 0xDA, 0x8F, 0x4E, 0x18, 0xEF, 0xD2
             , 0xC3, 0xC3, 0xD1, 0xEF, 0x1F, 0x60, 0xB5, 0x1E
             , 0x9C, 0x31, 0xDF, 0xA5, 0x87, 0x86, 0xA2, 0xDF
             , 0x3E, 0xC1, 0x6B, 0x3C, 0x39, 0x63, 0xBE, 0x4B
             , 0x0F, 0x0C, 0x45, 0xBF, 0x7D, 0x83, 0xD6, 0x79
             , 0x73, 0xC7, 0x7C, 0x97, 0x1E, 0x18, 0x8B, 0x7E
             , 0xFA, 0x06, 0xAC, 0xF3, 0xE6, 0x8F, 0xF8, 0xFC
             ]
    samePage freqlo

    freqhi <- label
    annotate "freqhi" $
        byte [ 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01
             , 0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x02, 0x02
             , 0x02, 0x02, 0x02, 0x03, 0x03, 0x03, 0x03, 0x03
             , 0x04, 0x04, 0x04, 0x04, 0x05, 0x05, 0x05, 0x06
             , 0x06, 0x07, 0x07, 0x07, 0x08, 0x08, 0x09, 0x09
             , 0x0A, 0x0B, 0x0B, 0x0C, 0x0D, 0x0E, 0x0E, 0x0F
             , 0x10, 0x11, 0x12, 0x13, 0x15, 0x16, 0x17, 0x19
             , 0x1A, 0x1C, 0x1D, 0x1F, 0x21, 0x23, 0x25, 0x27
             , 0x2A, 0x2C, 0x2F, 0x32, 0x35, 0x38, 0x3B, 0x3F
             , 0x43, 0x47, 0x4B, 0x4F, 0x54, 0x59, 0x5E, 0x64
             , 0x6A, 0x70, 0x77, 0x7E, 0x86, 0x8E, 0x96, 0x9F
             , 0xA8, 0xB3, 0xBD, 0xC8, 0xD4, 0xE1, 0xEE, 0xFD
             ]
    samePage freqhi

    arpeggio <- label
    annotate "arpeggio" $
        byte [0, 4, 7, 11, 7, 4, 0, 4]
    samePage arpeggio

    pure (toAddr afterXyzStep)


-- =========================================================================
-- Main entry point
-- =========================================================================

main :: IO ()
main = do
    let kernalRomPath  = Just "/usr/local/share/vice/C64/kernal-901227-03.bin"
        basicRomPath   = Just "/usr/local/share/vice/C64/basic-901226-01.bin"
        chargenRomPath = Just "/usr/local/share/vice/C64/chargen-906143-02.bin"
        subs = defaultC64Subsystems
            { useCassette = False, useRS232 = False, useKernalIRQ = False }
        cfg = (c64TargetConfig 0x0801 subs)
            { kernalRom = kernalRomPath, basicRom = basicRomPath, chargenRom = chargenRomPath }
        (afterXyzStepAddr, bytes, annotations, _labels) = assembleWithLabels cfg program
        prg = toPRG (origin cfg) bytes
        d64 = toD64 "ATTRAKTOR" prg
    BS.writeFile "attraktor.prg" (BS.pack prg)
    putStrLn $ "Wrote attraktor.prg (" ++ show (length prg) ++ " bytes)"
    BS.writeFile "attraktor.d64" (BS.pack d64)
    putStrLn $ "Wrote attraktor.d64 (" ++ show (length d64) ++ " bytes)"
    writeFile "attraktor.vs" (exportViceLabels annotations)
    putStrLn $ "Wrote attraktor.vs (" ++ show (Map.size annotations) ++ " labels)"
    writeFile "attraktor.asm" (exportAcmeWith cfg program)
    putStrLn "Wrote attraktor.asm"
    -- In VICE: attach disk image, then type  LOAD"*",8,1  followed by  RUN

    -- Execution profile: run the first 1M cycles through the emulator
    s0 <- loadC64Roms cfg initCPU
    let s1       = set regPC 0x080D $ loadProgram (origin cfg) bytes s0
        states   = traceForCycles 20_000_000 s1
        coverage = pcCoverage states
        labelMap = annotations `Map.union` romLabels
        profile  = formatProfile labelMap coverage
    writeFile "attraktor-profile.txt" profile
    putStrLn $ "Wrote attraktor-profile.txt (" ++ show (Map.size coverage) ++ " addresses)"

    -- Time-travel debug: find where we first jump to $0000
    let pairs = zip states (drop 1 states)
        transition = [(i, a, b) | ((a, b), i) <- zip pairs [0..],
                                   view regPC b == 0x0000,
                                   view regPC a /= 0x0000]
    case transition of
        (i, before, after):_ -> do
            putStrLn "\nFirst jump to $0000:"
            putStrLn $ formatState labelMap i before
            putStrLn $ formatState labelMap (i+1) after
        [] -> putStrLn "\nNo jump to $0000 found in trace"

    -- Write Lorenz attractor trajectory to text file
    let iterationStates = filter (\s -> view regPC s == afterXyzStepAddr) states
        header = "  Iter    Cycle        X           Y           Z"
        rows   = map (\(i, s) ->
                     "  " ++ padR 6 (show (i :: Int))
                  ++ "  " ++ padR 10 (show (view cycles s))
                  ++ "  " ++ padR 12 (showFFloat (Just 4) (readCBMFloat fpXcur s) "")
                  ++ "  " ++ padR 12 (showFFloat (Just 4) (readCBMFloat fpYcur s) "")
                  ++ "  " ++ showFFloat (Just 4) (readCBMFloat fpZcur s) "")
                 (zip [1..] iterationStates)
    writeFile "attraktor-trajectory.txt" (unlines (header : rows))
    putStrLn $ "Wrote attraktor-trajectory.txt (" ++ show (length iterationStates)
            ++ " iterations)"
