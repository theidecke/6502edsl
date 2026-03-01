module Emu.Step (execute, step) where

import Data.Bits ((.&.), (.|.), xor, complement, testBit, shiftL, shiftR)
import Data.Int (Int16)
import Data.Word (Word8, Word16)

import ISA.Mos6502
    ( Opcode(..), AddressingMode(..), Instruction(..)
    , decode, instrSize, baseCycles, canPageCross, lo, hi, w16
    )
import Emu.Mem (readByte)
import Emu.CPU

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Push a byte onto the stack ($0100 + SP), decrement SP.
push :: Word8 -> CPUState -> CPUState
push val s =
    let sp = view regSP s
        s' = set (memAt (0x0100 + fromIntegral sp)) val s
    in  set regSP (sp - 1) s'

-- | Pull a byte from the stack: increment SP, read ($0100 + SP).
pull :: CPUState -> (Word8, CPUState)
pull s =
    let sp = view regSP s + 1
        s' = set regSP sp s
        val = view (memAt (0x0100 + fromIntegral sp)) s'
    in  (val, s')

-- | Push a 16-bit value (high byte first, then low byte — so low is on top).
push16 :: Word16 -> CPUState -> CPUState
push16 w = push (lo w) . push (hi w)

-- | Pull a 16-bit value (low byte first from stack, then high).
pull16 :: CPUState -> (Word16, CPUState)
pull16 s =
    let (l, s1) = pull s
        (h, s2) = pull s1
    in  (w16 l h, s2)

-- | Does adding the index to the base cross a page boundary?
pageCross :: Word16 -> Word8 -> Bool
pageCross base idx = hi base /= hi (base + fromIntegral idx)

-- ---------------------------------------------------------------------------
-- Addressing mode resolution: returns (effective address, page crossed?)
-- ---------------------------------------------------------------------------

resolve :: AddressingMode -> CPUState -> (Word16, Bool)
resolve mode s = case mode of
    ZeroPage  a   -> (fromIntegral a, False)
    ZeroPageX a   -> (fromIntegral ((a + view regX s) .&. 0xFF), False)
    ZeroPageY a   -> (fromIntegral ((a + view regY s) .&. 0xFF), False)
    Absolute  a   -> (a, False)
    AbsoluteX a   -> (a + fromIntegral (view regX s),
                       pageCross a (view regX s))
    AbsoluteY a   -> (a + fromIntegral (view regY s),
                       pageCross a (view regY s))
    IndirectX a   -> let ptr = fromIntegral ((a + view regX s) .&. 0xFF) :: Word16
                         l   = view (memAt ptr) s
                         h   = view (memAt ((ptr + 1) .&. 0xFF)) s
                     in  (w16 l h, False)
    IndirectY a   -> let ptr = fromIntegral a :: Word16
                         l   = view (memAt ptr) s
                         h   = view (memAt ((ptr + 1) .&. 0xFF)) s
                         base = w16 l h
                         y   = view regY s
                     in  (base + fromIntegral y, pageCross base y)
    _ -> error $ "resolve: unexpected addressing mode " ++ show mode

-- | Read the effective operand value from the addressing mode.
readOperand :: AddressingMode -> CPUState -> (Word8, Bool)
readOperand (Immediate v) _ = (v, False)
readOperand (Accumulator) s = (view regA s, False)
readOperand mode s = let (addr, cross) = resolve mode s
                     in  (view (memAt addr) s, cross)

-- | Write a value to the effective address for the addressing mode.
writeResult :: AddressingMode -> Word8 -> CPUState -> CPUState
writeResult Accumulator val s = set regA val s
writeResult mode        val s = let (addr, _) = resolve mode s
                                in  set (memAt addr) val s

-- ---------------------------------------------------------------------------
-- Instruction execution
-- ---------------------------------------------------------------------------

-- | Execute a single decoded instruction. Advances PC by instrSize,
-- adds baseCycles + penalties, and performs the instruction's effect.
execute :: Instruction -> CPUState -> CPUState
execute instr@(Instruction opc mode) s0 =
    let isize   = fromIntegral (instrSize instr) :: Word16
        bCycles = baseCycles instr
        -- Advance PC past this instruction (control flow ops will override)
        s1      = over regPC (+ isize) s0
    in case opc of

    -- ---------------------------------------------------------------
    -- Load/Store
    -- ---------------------------------------------------------------
    LDA -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ updateNZ val $ set regA val s1

    LDX -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ updateNZ val $ set regX val s1

    LDY -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ updateNZ val $ set regY val s1

    STA -> let (addr, _) = resolve mode s1
           in  addCycles bCycles $ set (memAt addr) (view regA s1) s1

    STX -> let (addr, _) = resolve mode s1
           in  addCycles bCycles $ set (memAt addr) (view regX s1) s1

    STY -> let (addr, _) = resolve mode s1
           in  addCycles bCycles $ set (memAt addr) (view regY s1) s1

    -- ---------------------------------------------------------------
    -- Transfer
    -- ---------------------------------------------------------------
    TAX -> addCycles bCycles $ updateNZ v $ set regX v s1
      where v = view regA s1
    TAY -> addCycles bCycles $ updateNZ v $ set regY v s1
      where v = view regA s1
    TXA -> addCycles bCycles $ updateNZ v $ set regA v s1
      where v = view regX s1
    TYA -> addCycles bCycles $ updateNZ v $ set regA v s1
      where v = view regY s1
    TSX -> addCycles bCycles $ updateNZ v $ set regX v s1
      where v = view regSP s1
    TXS -> addCycles bCycles $ set regSP (view regX s1) s1  -- no N/Z update!

    -- ---------------------------------------------------------------
    -- Stack
    -- ---------------------------------------------------------------
    PHA -> addCycles bCycles $ push (view regA s1) s1
    PHP -> addCycles bCycles $ push (view regP s1 .|. 0x30) s1  -- B + bit5
    PLA -> let (val, s2) = pull s1
           in  addCycles bCycles $ updateNZ val $ set regA val s2
    PLP -> let (val, s2) = pull s1
           in  addCycles bCycles $ set regP ((val .|. 0x20) .&. complement 0x10) s2

    -- ---------------------------------------------------------------
    -- Arithmetic
    -- ---------------------------------------------------------------
    ADC -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ doADC val s1

    SBC -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ doSBC val s1

    -- ---------------------------------------------------------------
    -- Logic
    -- ---------------------------------------------------------------
    AND -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
               result = view regA s1 .&. val
           in  addCycles (bCycles + penalty) $ updateNZ result $ set regA result s1

    ORA -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
               result = view regA s1 .|. val
           in  addCycles (bCycles + penalty) $ updateNZ result $ set regA result s1

    EOR -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
               result = view regA s1 `xor` val
           in  addCycles (bCycles + penalty) $ updateNZ result $ set regA result s1

    -- ---------------------------------------------------------------
    -- Compare
    -- ---------------------------------------------------------------
    CMP -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ doCMP (view regA s1) val s1

    CPX -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ doCMP (view regX s1) val s1

    CPY -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $ doCMP (view regY s1) val s1

    -- ---------------------------------------------------------------
    -- Shift and Rotate
    -- ---------------------------------------------------------------
    ASL -> let (val, _) = readOperand mode s1
               c   = testBit val 7
               res = val `shiftL` 1
           in  addCycles bCycles $ set flagC c $ updateNZ res $ writeResult mode res s1

    LSR -> let (val, _) = readOperand mode s1
               c   = testBit val 0
               res = val `shiftR` 1
           in  addCycles bCycles $ set flagC c $ updateNZ res $ writeResult mode res s1

    ROL -> let (val, _) = readOperand mode s1
               oldC = if view flagC s1 then 1 else 0
               c    = testBit val 7
               res  = (val `shiftL` 1) .|. oldC
           in  addCycles bCycles $ set flagC c $ updateNZ res $ writeResult mode res s1

    ROR -> let (val, _) = readOperand mode s1
               oldC = if view flagC s1 then 0x80 else 0
               c    = testBit val 0
               res  = (val `shiftR` 1) .|. oldC
           in  addCycles bCycles $ set flagC c $ updateNZ res $ writeResult mode res s1

    -- ---------------------------------------------------------------
    -- Increment / Decrement
    -- ---------------------------------------------------------------
    INC -> let (val, _) = readOperand mode s1
               res = val + 1
           in  addCycles bCycles $ updateNZ res $ writeResult mode res s1

    DEC -> let (val, _) = readOperand mode s1
               res = val - 1
           in  addCycles bCycles $ updateNZ res $ writeResult mode res s1

    INX -> addCycles bCycles $ updateNZ res $ set regX res s1
      where res = view regX s1 + 1
    DEX -> addCycles bCycles $ updateNZ res $ set regX res s1
      where res = view regX s1 - 1
    INY -> addCycles bCycles $ updateNZ res $ set regY res s1
      where res = view regY s1 + 1
    DEY -> addCycles bCycles $ updateNZ res $ set regY res s1
      where res = view regY s1 - 1

    -- ---------------------------------------------------------------
    -- Branches
    -- ---------------------------------------------------------------
    BCC -> doBranch (not (view flagC s1)) mode s1 bCycles
    BCS -> doBranch (view flagC s1)       mode s1 bCycles
    BEQ -> doBranch (view flagZ s1)       mode s1 bCycles
    BNE -> doBranch (not (view flagZ s1)) mode s1 bCycles
    BMI -> doBranch (view flagN s1)       mode s1 bCycles
    BPL -> doBranch (not (view flagN s1)) mode s1 bCycles
    BVS -> doBranch (view flagV s1)       mode s1 bCycles
    BVC -> doBranch (not (view flagV s1)) mode s1 bCycles

    -- ---------------------------------------------------------------
    -- Jump / Call / Return
    -- ---------------------------------------------------------------
    JMP -> case mode of
        Absolute a  -> addCycles bCycles $ set regPC a s1
        Indirect a  ->
            -- 6502 page-boundary bug: if lo byte is 0xFF,
            -- high byte comes from addr & 0xFF00, not addr+1
            let l = readByte a (view mem s1)
                hAddr = (a .&. 0xFF00) .|. fromIntegral ((lo a + 1) .&. 0xFF)
                h = readByte hAddr (view mem s1)
            in  addCycles bCycles $ set regPC (w16 l h) s1
        _ -> error "JMP: invalid addressing mode"

    JSR -> case mode of
        Absolute a ->
            -- Push PC-1 (return address - 1; RTS adds 1)
            let retAddr = view regPC s1 - 1
            in  addCycles bCycles $ set regPC a $ push16 retAddr s1
        _ -> error "JSR: invalid addressing mode"

    RTS -> let (retAddr, s2) = pull16 s1
           in  addCycles bCycles $ set regPC (retAddr + 1) s2

    RTI -> let (p, s2)    = pull s1
               (addr, s3) = pull16 s2
           in  addCycles bCycles $ set regPC addr $
               set regP ((p .|. 0x20) .&. complement 0x10) s3

    -- ---------------------------------------------------------------
    -- BRK
    -- ---------------------------------------------------------------
    BRK -> let pc2  = view regPC s0 + 2  -- BRK pushes PC+2
               pVal = view regP s1 .|. 0x30  -- B flag + bit5 set on push
               s2   = push16 pc2 s1
               s3   = push pVal s2
               vec  = w16 (readByte 0xFFFE (view mem s3))
                          (readByte 0xFFFF (view mem s3))
           in  addCycles bCycles $ set flagI True $ set regPC vec s3

    -- ---------------------------------------------------------------
    -- Flags
    -- ---------------------------------------------------------------
    CLC -> addCycles bCycles $ set flagC False s1
    SEC -> addCycles bCycles $ set flagC True  s1
    CLD -> addCycles bCycles $ set flagD False s1
    SED -> addCycles bCycles $ set flagD True  s1
    CLI -> addCycles bCycles $ set flagI False s1
    SEI -> addCycles bCycles $ set flagI True  s1
    CLV -> addCycles bCycles $ set flagV False s1

    -- ---------------------------------------------------------------
    -- BIT
    -- ---------------------------------------------------------------
    BIT -> let (val, cross) = readOperand mode s1
               penalty = if canPageCross instr && cross then 1 else 0
           in  addCycles (bCycles + penalty) $
               set flagZ ((view regA s1 .&. val) == 0) $
               set flagV (testBit val 6) $
               set flagN (testBit val 7) s1

    -- ---------------------------------------------------------------
    -- NOP
    -- ---------------------------------------------------------------
    NOP -> addCycles bCycles s1

-- ---------------------------------------------------------------------------
-- ADC / SBC helpers
-- ---------------------------------------------------------------------------

doADC :: Word8 -> CPUState -> CPUState
doADC val s
    | view flagD s = doADC_BCD val s
    | otherwise    = doADC_BIN val s

doADC_BIN :: Word8 -> CPUState -> CPUState
doADC_BIN val s =
    let a    = view regA s
        c    = if view flagC s then 1 else 0 :: Word16
        sum16 = fromIntegral a + fromIntegral val + c :: Word16
        res   = fromIntegral sum16 :: Word8
        -- Overflow: sign of result differs from both inputs
        ovf   = (complement (fromIntegral a `xor` fromIntegral val) .&.
                 (fromIntegral a `xor` sum16)) .&. 0x80 /= 0
    in  set flagC (sum16 > 0xFF) $
        set flagV ovf $
        updateNZ res $
        set regA res s

doADC_BCD :: Word8 -> CPUState -> CPUState
doADC_BCD val s =
    let a    = view regA s
        c    = if view flagC s then 1 else 0 :: Int
        -- Binary result for N/Z/V flags (NMOS behavior)
        binSum = fromIntegral a + fromIntegral val + fromIntegral c :: Word16
        binRes = fromIntegral binSum :: Word8
        ovf    = (complement (fromIntegral a `xor` fromIntegral val) .&.
                  (fromIntegral a `xor` binSum)) .&. 0x80 /= 0
        -- BCD arithmetic
        loNib  = (fromIntegral a .&. 0x0F) + (fromIntegral val .&. 0x0F) + c :: Int
        loAdj  = if loNib > 9 then loNib + 6 else loNib
        hiNib  = (fromIntegral a .&. 0xF0) + (fromIntegral val .&. 0xF0) + (if loAdj > 0x0F then 0x10 else 0) :: Int
        hiAdj  = if hiNib > 0x90 then hiNib + 0x60 else hiNib
        res    = fromIntegral ((loAdj .&. 0x0F) .|. (hiAdj .&. 0xF0)) :: Word8
        carry  = hiAdj > 0x90 -- 6502 quirk: carry from BCD, not binary
    in  set flagC carry $
        set flagV ovf $
        updateNZ binRes $  -- N/Z from binary result on NMOS
        set regA res s

doSBC :: Word8 -> CPUState -> CPUState
doSBC val s
    | view flagD s = doSBC_BCD val s
    | otherwise    = doSBC_BIN val s

doSBC_BIN :: Word8 -> CPUState -> CPUState
doSBC_BIN val s =
    let a    = view regA s
        c    = if view flagC s then 0 else 1 :: Word16
        diff = fromIntegral a - fromIntegral val - c :: Word16
        res  = fromIntegral diff :: Word8
        ovf  = ((fromIntegral a `xor` fromIntegral val) .&.
                (fromIntegral a `xor` diff)) .&. 0x80 /= 0
    in  set flagC (diff < 0x100) $
        set flagV ovf $
        updateNZ res $
        set regA res s

doSBC_BCD :: Word8 -> CPUState -> CPUState
doSBC_BCD val s =
    let a    = view regA s
        c    = if view flagC s then 0 else 1 :: Int
        -- Binary result for N/Z/V (NMOS behavior)
        binDiff = fromIntegral a - fromIntegral val - fromIntegral c :: Word16
        binRes  = fromIntegral binDiff :: Word8
        ovf     = ((fromIntegral a `xor` fromIntegral val) .&.
                   (fromIntegral a `xor` binDiff)) .&. 0x80 /= 0
        binCarry = binDiff < 0x100
        -- BCD arithmetic
        loNib   = (fromIntegral a .&. 0x0F) - (fromIntegral val .&. 0x0F) - c :: Int
        loAdj   = if loNib < 0 then loNib + 10 else loNib
        borrow  = if loNib < 0 then 1 else 0 :: Int
        hiNib   = (fromIntegral a `shiftR` 4) - (fromIntegral val `shiftR` 4) - borrow :: Int
        hiAdj   = if hiNib < 0 then hiNib + 10 else hiNib
        res     = fromIntegral ((hiAdj `shiftL` 4) .|. (loAdj .&. 0x0F)) :: Word8
    in  set flagC binCarry $
        set flagV ovf $
        updateNZ binRes $  -- N/Z from binary result on NMOS
        set regA res s

-- ---------------------------------------------------------------------------
-- Compare helper
-- ---------------------------------------------------------------------------

doCMP :: Word8 -> Word8 -> CPUState -> CPUState
doCMP reg val s =
    let diff = fromIntegral reg - fromIntegral val :: Word16
        res  = fromIntegral diff :: Word8
    in  set flagC (reg >= val) $ updateNZ res s

-- ---------------------------------------------------------------------------
-- Branch helper
-- ---------------------------------------------------------------------------

doBranch :: Bool -> AddressingMode -> CPUState -> Int -> CPUState
doBranch taken (Relative offset) s bCyc
    | not taken = addCycles bCyc s
    | otherwise =
        let pc     = view regPC s
            -- Sign-extend Int8 offset via Int16 to get correct Word16 addition
            target = pc + fromIntegral (fromIntegral offset :: Int16)
            cross  = hi pc /= hi target
            penalty = 1 + if cross then 1 else 0
        in  addCycles (bCyc + penalty) $ set regPC target s
doBranch _ _ _ _ = error "doBranch: not a relative mode"

-- ---------------------------------------------------------------------------
-- Cycle accounting
-- ---------------------------------------------------------------------------

addCycles :: Int -> CPUState -> CPUState
addCycles n = over cycles (+ n)

-- ---------------------------------------------------------------------------
-- Fetch-Decode-Execute
-- ---------------------------------------------------------------------------

-- | Single step: fetch opcode at PC, decode, execute.
step :: CPUState -> CPUState
step s =
    let pc  = view regPC s
        op  = view (memAt pc) s
    in case decode op of
        Nothing     -> error $ "Illegal opcode: 0x" ++ showHex8 op
        Just (0, f) -> execute (f 0 0) s
        Just (1, f) -> let b1 = view (memAt (pc + 1)) s
                       in  execute (f b1 0) s
        Just (2, f) -> let b1 = view (memAt (pc + 1)) s
                           b2 = view (memAt (pc + 2)) s
                       in  execute (f b1 b2) s
        Just _      -> error "Unexpected operand count"

showHex8 :: Word8 -> String
showHex8 w = [hexDigit (w `shiftR` 4), hexDigit (w .&. 0x0F)]
  where
    hexDigit n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
               | otherwise = toEnum (fromEnum 'A' + fromIntegral n - 10)
