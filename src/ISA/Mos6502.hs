module ISA.Mos6502
    ( Opcode(..)
    , AddressingMode(..)
    , Instruction(..)
    , encode
    , decode
    , instrSize
    , baseCycles
    , canPageCross
    ) where

import Data.Array (Array, listArray, (!))
import Data.Bits  (shiftR)
import Data.Int   (Int8)
import Data.Word  (Word8, Word16)

-- | All 56 official 6502 mnemonics.
data Opcode
    = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
    | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
    | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
    | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
    | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
    | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
    | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | 6502 addressing modes.
data AddressingMode
    = Implied
    | Accumulator
    | Immediate  Word8
    | ZeroPage   Word8
    | ZeroPageX  Word8
    | ZeroPageY  Word8
    | Absolute   Word16
    | AbsoluteX  Word16
    | AbsoluteY  Word16
    | Indirect   Word16   -- JMP only
    | IndirectX  Word8    -- indexed indirect, (zp,X)
    | IndirectY  Word8    -- indirect indexed, (zp),Y
    | Relative   Int8     -- branches
    deriving (Show, Eq)

-- | A single 6502 instruction: an opcode paired with its addressing mode.
data Instruction = Instruction Opcode AddressingMode
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Internal helpers (dependency-free lo/hi)
-- ---------------------------------------------------------------------------

lo :: Word16 -> Word8
lo = fromIntegral

hi :: Word16 -> Word8
hi w = fromIntegral (w `shiftR` 8)

-- ---------------------------------------------------------------------------
-- encode: Instruction -> [Word8]
-- ---------------------------------------------------------------------------

encode :: Instruction -> [Word8]
-- ADC
encode (Instruction ADC (Immediate v))  = [0x69, v]
encode (Instruction ADC (ZeroPage a))   = [0x65, a]
encode (Instruction ADC (ZeroPageX a))  = [0x75, a]
encode (Instruction ADC (Absolute a))   = [0x6D, lo a, hi a]
encode (Instruction ADC (AbsoluteX a))  = [0x7D, lo a, hi a]
encode (Instruction ADC (AbsoluteY a))  = [0x79, lo a, hi a]
encode (Instruction ADC (IndirectX a))  = [0x61, a]
encode (Instruction ADC (IndirectY a))  = [0x71, a]
-- AND
encode (Instruction AND (Immediate v))  = [0x29, v]
encode (Instruction AND (ZeroPage a))   = [0x25, a]
encode (Instruction AND (ZeroPageX a))  = [0x35, a]
encode (Instruction AND (Absolute a))   = [0x2D, lo a, hi a]
encode (Instruction AND (AbsoluteX a))  = [0x3D, lo a, hi a]
encode (Instruction AND (AbsoluteY a))  = [0x39, lo a, hi a]
encode (Instruction AND (IndirectX a))  = [0x21, a]
encode (Instruction AND (IndirectY a))  = [0x31, a]
-- ASL
encode (Instruction ASL Accumulator)    = [0x0A]
encode (Instruction ASL (ZeroPage a))   = [0x06, a]
encode (Instruction ASL (ZeroPageX a))  = [0x16, a]
encode (Instruction ASL (Absolute a))   = [0x0E, lo a, hi a]
encode (Instruction ASL (AbsoluteX a))  = [0x1E, lo a, hi a]
-- BCC..BVS (branches)
encode (Instruction BCC (Relative r))   = [0x90, fromIntegral r]
encode (Instruction BCS (Relative r))   = [0xB0, fromIntegral r]
encode (Instruction BEQ (Relative r))   = [0xF0, fromIntegral r]
encode (Instruction BMI (Relative r))   = [0x30, fromIntegral r]
encode (Instruction BNE (Relative r))   = [0xD0, fromIntegral r]
encode (Instruction BPL (Relative r))   = [0x10, fromIntegral r]
encode (Instruction BVC (Relative r))   = [0x50, fromIntegral r]
encode (Instruction BVS (Relative r))   = [0x70, fromIntegral r]
-- BIT
encode (Instruction BIT (ZeroPage a))   = [0x24, a]
encode (Instruction BIT (Absolute a))   = [0x2C, lo a, hi a]
-- BRK
encode (Instruction BRK Implied)        = [0x00]
-- CLC, CLD, CLI, CLV
encode (Instruction CLC Implied)        = [0x18]
encode (Instruction CLD Implied)        = [0xD8]
encode (Instruction CLI Implied)        = [0x58]
encode (Instruction CLV Implied)        = [0xB8]
-- CMP
encode (Instruction CMP (Immediate v))  = [0xC9, v]
encode (Instruction CMP (ZeroPage a))   = [0xC5, a]
encode (Instruction CMP (ZeroPageX a))  = [0xD5, a]
encode (Instruction CMP (Absolute a))   = [0xCD, lo a, hi a]
encode (Instruction CMP (AbsoluteX a))  = [0xDD, lo a, hi a]
encode (Instruction CMP (AbsoluteY a))  = [0xD9, lo a, hi a]
encode (Instruction CMP (IndirectX a))  = [0xC1, a]
encode (Instruction CMP (IndirectY a))  = [0xD1, a]
-- CPX
encode (Instruction CPX (Immediate v))  = [0xE0, v]
encode (Instruction CPX (ZeroPage a))   = [0xE4, a]
encode (Instruction CPX (Absolute a))   = [0xEC, lo a, hi a]
-- CPY
encode (Instruction CPY (Immediate v))  = [0xC0, v]
encode (Instruction CPY (ZeroPage a))   = [0xC4, a]
encode (Instruction CPY (Absolute a))   = [0xCC, lo a, hi a]
-- DEC
encode (Instruction DEC (ZeroPage a))   = [0xC6, a]
encode (Instruction DEC (ZeroPageX a))  = [0xD6, a]
encode (Instruction DEC (Absolute a))   = [0xCE, lo a, hi a]
encode (Instruction DEC (AbsoluteX a))  = [0xDE, lo a, hi a]
-- DEX, DEY
encode (Instruction DEX Implied)        = [0xCA]
encode (Instruction DEY Implied)        = [0x88]
-- EOR
encode (Instruction EOR (Immediate v))  = [0x49, v]
encode (Instruction EOR (ZeroPage a))   = [0x45, a]
encode (Instruction EOR (ZeroPageX a))  = [0x55, a]
encode (Instruction EOR (Absolute a))   = [0x4D, lo a, hi a]
encode (Instruction EOR (AbsoluteX a))  = [0x5D, lo a, hi a]
encode (Instruction EOR (AbsoluteY a))  = [0x59, lo a, hi a]
encode (Instruction EOR (IndirectX a))  = [0x41, a]
encode (Instruction EOR (IndirectY a))  = [0x51, a]
-- INC
encode (Instruction INC (ZeroPage a))   = [0xE6, a]
encode (Instruction INC (ZeroPageX a))  = [0xF6, a]
encode (Instruction INC (Absolute a))   = [0xEE, lo a, hi a]
encode (Instruction INC (AbsoluteX a))  = [0xFE, lo a, hi a]
-- INX, INY
encode (Instruction INX Implied)        = [0xE8]
encode (Instruction INY Implied)        = [0xC8]
-- JMP
encode (Instruction JMP (Absolute a))   = [0x4C, lo a, hi a]
encode (Instruction JMP (Indirect a))   = [0x6C, lo a, hi a]
-- JSR
encode (Instruction JSR (Absolute a))   = [0x20, lo a, hi a]
-- LDA
encode (Instruction LDA (Immediate v))  = [0xA9, v]
encode (Instruction LDA (ZeroPage a))   = [0xA5, a]
encode (Instruction LDA (ZeroPageX a))  = [0xB5, a]
encode (Instruction LDA (Absolute a))   = [0xAD, lo a, hi a]
encode (Instruction LDA (AbsoluteX a))  = [0xBD, lo a, hi a]
encode (Instruction LDA (AbsoluteY a))  = [0xB9, lo a, hi a]
encode (Instruction LDA (IndirectX a))  = [0xA1, a]
encode (Instruction LDA (IndirectY a))  = [0xB1, a]
-- LDX
encode (Instruction LDX (Immediate v))  = [0xA2, v]
encode (Instruction LDX (ZeroPage a))   = [0xA6, a]
encode (Instruction LDX (ZeroPageY a))  = [0xB6, a]
encode (Instruction LDX (Absolute a))   = [0xAE, lo a, hi a]
encode (Instruction LDX (AbsoluteY a))  = [0xBE, lo a, hi a]
-- LDY
encode (Instruction LDY (Immediate v))  = [0xA0, v]
encode (Instruction LDY (ZeroPage a))   = [0xA4, a]
encode (Instruction LDY (ZeroPageX a))  = [0xB4, a]
encode (Instruction LDY (Absolute a))   = [0xAC, lo a, hi a]
encode (Instruction LDY (AbsoluteX a))  = [0xBC, lo a, hi a]
-- LSR
encode (Instruction LSR Accumulator)    = [0x4A]
encode (Instruction LSR (ZeroPage a))   = [0x46, a]
encode (Instruction LSR (ZeroPageX a))  = [0x56, a]
encode (Instruction LSR (Absolute a))   = [0x4E, lo a, hi a]
encode (Instruction LSR (AbsoluteX a))  = [0x5E, lo a, hi a]
-- NOP
encode (Instruction NOP Implied)        = [0xEA]
-- ORA
encode (Instruction ORA (Immediate v))  = [0x09, v]
encode (Instruction ORA (ZeroPage a))   = [0x05, a]
encode (Instruction ORA (ZeroPageX a))  = [0x15, a]
encode (Instruction ORA (Absolute a))   = [0x0D, lo a, hi a]
encode (Instruction ORA (AbsoluteX a))  = [0x1D, lo a, hi a]
encode (Instruction ORA (AbsoluteY a))  = [0x19, lo a, hi a]
encode (Instruction ORA (IndirectX a))  = [0x01, a]
encode (Instruction ORA (IndirectY a))  = [0x11, a]
-- PHA, PHP, PLA, PLP
encode (Instruction PHA Implied)        = [0x48]
encode (Instruction PHP Implied)        = [0x08]
encode (Instruction PLA Implied)        = [0x68]
encode (Instruction PLP Implied)        = [0x28]
-- ROL
encode (Instruction ROL Accumulator)    = [0x2A]
encode (Instruction ROL (ZeroPage a))   = [0x26, a]
encode (Instruction ROL (ZeroPageX a))  = [0x36, a]
encode (Instruction ROL (Absolute a))   = [0x2E, lo a, hi a]
encode (Instruction ROL (AbsoluteX a))  = [0x3E, lo a, hi a]
-- ROR
encode (Instruction ROR Accumulator)    = [0x6A]
encode (Instruction ROR (ZeroPage a))   = [0x66, a]
encode (Instruction ROR (ZeroPageX a))  = [0x76, a]
encode (Instruction ROR (Absolute a))   = [0x6E, lo a, hi a]
encode (Instruction ROR (AbsoluteX a))  = [0x7E, lo a, hi a]
-- RTI, RTS
encode (Instruction RTI Implied)        = [0x40]
encode (Instruction RTS Implied)        = [0x60]
-- SBC
encode (Instruction SBC (Immediate v))  = [0xE9, v]
encode (Instruction SBC (ZeroPage a))   = [0xE5, a]
encode (Instruction SBC (ZeroPageX a))  = [0xF5, a]
encode (Instruction SBC (Absolute a))   = [0xED, lo a, hi a]
encode (Instruction SBC (AbsoluteX a))  = [0xFD, lo a, hi a]
encode (Instruction SBC (AbsoluteY a))  = [0xF9, lo a, hi a]
encode (Instruction SBC (IndirectX a))  = [0xE1, a]
encode (Instruction SBC (IndirectY a))  = [0xF1, a]
-- SEC, SED, SEI
encode (Instruction SEC Implied)        = [0x38]
encode (Instruction SED Implied)        = [0xF8]
encode (Instruction SEI Implied)        = [0x78]
-- STA
encode (Instruction STA (ZeroPage a))   = [0x85, a]
encode (Instruction STA (ZeroPageX a))  = [0x95, a]
encode (Instruction STA (Absolute a))   = [0x8D, lo a, hi a]
encode (Instruction STA (AbsoluteX a))  = [0x9D, lo a, hi a]
encode (Instruction STA (AbsoluteY a))  = [0x99, lo a, hi a]
encode (Instruction STA (IndirectX a))  = [0x81, a]
encode (Instruction STA (IndirectY a))  = [0x91, a]
-- STX
encode (Instruction STX (ZeroPage a))   = [0x86, a]
encode (Instruction STX (ZeroPageY a))  = [0x96, a]
encode (Instruction STX (Absolute a))   = [0x8E, lo a, hi a]
-- STY
encode (Instruction STY (ZeroPage a))   = [0x84, a]
encode (Instruction STY (ZeroPageX a))  = [0x94, a]
encode (Instruction STY (Absolute a))   = [0x8C, lo a, hi a]
-- TAX, TAY, TSX, TXA, TXS, TYA
encode (Instruction TAX Implied)        = [0xAA]
encode (Instruction TAY Implied)        = [0xA8]
encode (Instruction TSX Implied)        = [0xBA]
encode (Instruction TXA Implied)        = [0x8A]
encode (Instruction TXS Implied)        = [0x9A]
encode (Instruction TYA Implied)        = [0x98]
-- Invalid
encode (Instruction opc mode) =
    error $ "Invalid addressing mode for " ++ show opc ++ ": " ++ show mode

-- ---------------------------------------------------------------------------
-- instrSize: byte count from AddressingMode constructor (never forces operands)
-- ---------------------------------------------------------------------------

instrSize :: Instruction -> Int
instrSize (Instruction _ mode) = case mode of
    Implied      -> 1
    Accumulator  -> 1
    Immediate{}  -> 2
    ZeroPage{}   -> 2
    ZeroPageX{}  -> 2
    ZeroPageY{}  -> 2
    Relative{}   -> 2
    IndirectX{}  -> 2
    IndirectY{}  -> 2
    Absolute{}   -> 3
    AbsoluteX{}  -> 3
    AbsoluteY{}  -> 3
    Indirect{}   -> 3

-- ---------------------------------------------------------------------------
-- decode: Word8 -> Maybe (Int, Word8 -> Word8 -> Instruction)
-- ---------------------------------------------------------------------------

-- | Decode an opcode byte. Returns the number of operand bytes (0, 1, or 2)
-- and a continuation that takes up to 2 operand bytes and yields an Instruction.
-- Unused operand bytes are ignored by the continuation.
decode :: Word8 -> Maybe (Int, Word8 -> Word8 -> Instruction)
decode opByte = decodeTable ! opByte

-- | Internal list of all 151 valid (opcode byte, operand count, constructor) triples.
decodeEntries :: [(Word8, Int, Word8 -> Word8 -> Instruction)]
decodeEntries =
    -- ADC
    [ (0x69, 1, \b _ -> Instruction ADC (Immediate b))
    , (0x65, 1, \b _ -> Instruction ADC (ZeroPage b))
    , (0x75, 1, \b _ -> Instruction ADC (ZeroPageX b))
    , (0x6D, 2, \l h -> Instruction ADC (Absolute (w16 l h)))
    , (0x7D, 2, \l h -> Instruction ADC (AbsoluteX (w16 l h)))
    , (0x79, 2, \l h -> Instruction ADC (AbsoluteY (w16 l h)))
    , (0x61, 1, \b _ -> Instruction ADC (IndirectX b))
    , (0x71, 1, \b _ -> Instruction ADC (IndirectY b))
    -- AND
    , (0x29, 1, \b _ -> Instruction AND (Immediate b))
    , (0x25, 1, \b _ -> Instruction AND (ZeroPage b))
    , (0x35, 1, \b _ -> Instruction AND (ZeroPageX b))
    , (0x2D, 2, \l h -> Instruction AND (Absolute (w16 l h)))
    , (0x3D, 2, \l h -> Instruction AND (AbsoluteX (w16 l h)))
    , (0x39, 2, \l h -> Instruction AND (AbsoluteY (w16 l h)))
    , (0x21, 1, \b _ -> Instruction AND (IndirectX b))
    , (0x31, 1, \b _ -> Instruction AND (IndirectY b))
    -- ASL
    , (0x0A, 0, \_ _ -> Instruction ASL Accumulator)
    , (0x06, 1, \b _ -> Instruction ASL (ZeroPage b))
    , (0x16, 1, \b _ -> Instruction ASL (ZeroPageX b))
    , (0x0E, 2, \l h -> Instruction ASL (Absolute (w16 l h)))
    , (0x1E, 2, \l h -> Instruction ASL (AbsoluteX (w16 l h)))
    -- BCC..BVS
    , (0x90, 1, \b _ -> Instruction BCC (Relative (fromIntegral b)))
    , (0xB0, 1, \b _ -> Instruction BCS (Relative (fromIntegral b)))
    , (0xF0, 1, \b _ -> Instruction BEQ (Relative (fromIntegral b)))
    , (0x30, 1, \b _ -> Instruction BMI (Relative (fromIntegral b)))
    , (0xD0, 1, \b _ -> Instruction BNE (Relative (fromIntegral b)))
    , (0x10, 1, \b _ -> Instruction BPL (Relative (fromIntegral b)))
    , (0x50, 1, \b _ -> Instruction BVC (Relative (fromIntegral b)))
    , (0x70, 1, \b _ -> Instruction BVS (Relative (fromIntegral b)))
    -- BIT
    , (0x24, 1, \b _ -> Instruction BIT (ZeroPage b))
    , (0x2C, 2, \l h -> Instruction BIT (Absolute (w16 l h)))
    -- BRK
    , (0x00, 0, \_ _ -> Instruction BRK Implied)
    -- CLC, CLD, CLI, CLV
    , (0x18, 0, \_ _ -> Instruction CLC Implied)
    , (0xD8, 0, \_ _ -> Instruction CLD Implied)
    , (0x58, 0, \_ _ -> Instruction CLI Implied)
    , (0xB8, 0, \_ _ -> Instruction CLV Implied)
    -- CMP
    , (0xC9, 1, \b _ -> Instruction CMP (Immediate b))
    , (0xC5, 1, \b _ -> Instruction CMP (ZeroPage b))
    , (0xD5, 1, \b _ -> Instruction CMP (ZeroPageX b))
    , (0xCD, 2, \l h -> Instruction CMP (Absolute (w16 l h)))
    , (0xDD, 2, \l h -> Instruction CMP (AbsoluteX (w16 l h)))
    , (0xD9, 2, \l h -> Instruction CMP (AbsoluteY (w16 l h)))
    , (0xC1, 1, \b _ -> Instruction CMP (IndirectX b))
    , (0xD1, 1, \b _ -> Instruction CMP (IndirectY b))
    -- CPX
    , (0xE0, 1, \b _ -> Instruction CPX (Immediate b))
    , (0xE4, 1, \b _ -> Instruction CPX (ZeroPage b))
    , (0xEC, 2, \l h -> Instruction CPX (Absolute (w16 l h)))
    -- CPY
    , (0xC0, 1, \b _ -> Instruction CPY (Immediate b))
    , (0xC4, 1, \b _ -> Instruction CPY (ZeroPage b))
    , (0xCC, 2, \l h -> Instruction CPY (Absolute (w16 l h)))
    -- DEC
    , (0xC6, 1, \b _ -> Instruction DEC (ZeroPage b))
    , (0xD6, 1, \b _ -> Instruction DEC (ZeroPageX b))
    , (0xCE, 2, \l h -> Instruction DEC (Absolute (w16 l h)))
    , (0xDE, 2, \l h -> Instruction DEC (AbsoluteX (w16 l h)))
    -- DEX, DEY
    , (0xCA, 0, \_ _ -> Instruction DEX Implied)
    , (0x88, 0, \_ _ -> Instruction DEY Implied)
    -- EOR
    , (0x49, 1, \b _ -> Instruction EOR (Immediate b))
    , (0x45, 1, \b _ -> Instruction EOR (ZeroPage b))
    , (0x55, 1, \b _ -> Instruction EOR (ZeroPageX b))
    , (0x4D, 2, \l h -> Instruction EOR (Absolute (w16 l h)))
    , (0x5D, 2, \l h -> Instruction EOR (AbsoluteX (w16 l h)))
    , (0x59, 2, \l h -> Instruction EOR (AbsoluteY (w16 l h)))
    , (0x41, 1, \b _ -> Instruction EOR (IndirectX b))
    , (0x51, 1, \b _ -> Instruction EOR (IndirectY b))
    -- INC
    , (0xE6, 1, \b _ -> Instruction INC (ZeroPage b))
    , (0xF6, 1, \b _ -> Instruction INC (ZeroPageX b))
    , (0xEE, 2, \l h -> Instruction INC (Absolute (w16 l h)))
    , (0xFE, 2, \l h -> Instruction INC (AbsoluteX (w16 l h)))
    -- INX, INY
    , (0xE8, 0, \_ _ -> Instruction INX Implied)
    , (0xC8, 0, \_ _ -> Instruction INY Implied)
    -- JMP
    , (0x4C, 2, \l h -> Instruction JMP (Absolute (w16 l h)))
    , (0x6C, 2, \l h -> Instruction JMP (Indirect (w16 l h)))
    -- JSR
    , (0x20, 2, \l h -> Instruction JSR (Absolute (w16 l h)))
    -- LDA
    , (0xA9, 1, \b _ -> Instruction LDA (Immediate b))
    , (0xA5, 1, \b _ -> Instruction LDA (ZeroPage b))
    , (0xB5, 1, \b _ -> Instruction LDA (ZeroPageX b))
    , (0xAD, 2, \l h -> Instruction LDA (Absolute (w16 l h)))
    , (0xBD, 2, \l h -> Instruction LDA (AbsoluteX (w16 l h)))
    , (0xB9, 2, \l h -> Instruction LDA (AbsoluteY (w16 l h)))
    , (0xA1, 1, \b _ -> Instruction LDA (IndirectX b))
    , (0xB1, 1, \b _ -> Instruction LDA (IndirectY b))
    -- LDX
    , (0xA2, 1, \b _ -> Instruction LDX (Immediate b))
    , (0xA6, 1, \b _ -> Instruction LDX (ZeroPage b))
    , (0xB6, 1, \b _ -> Instruction LDX (ZeroPageY b))
    , (0xAE, 2, \l h -> Instruction LDX (Absolute (w16 l h)))
    , (0xBE, 2, \l h -> Instruction LDX (AbsoluteY (w16 l h)))
    -- LDY
    , (0xA0, 1, \b _ -> Instruction LDY (Immediate b))
    , (0xA4, 1, \b _ -> Instruction LDY (ZeroPage b))
    , (0xB4, 1, \b _ -> Instruction LDY (ZeroPageX b))
    , (0xAC, 2, \l h -> Instruction LDY (Absolute (w16 l h)))
    , (0xBC, 2, \l h -> Instruction LDY (AbsoluteX (w16 l h)))
    -- LSR
    , (0x4A, 0, \_ _ -> Instruction LSR Accumulator)
    , (0x46, 1, \b _ -> Instruction LSR (ZeroPage b))
    , (0x56, 1, \b _ -> Instruction LSR (ZeroPageX b))
    , (0x4E, 2, \l h -> Instruction LSR (Absolute (w16 l h)))
    , (0x5E, 2, \l h -> Instruction LSR (AbsoluteX (w16 l h)))
    -- NOP
    , (0xEA, 0, \_ _ -> Instruction NOP Implied)
    -- ORA
    , (0x09, 1, \b _ -> Instruction ORA (Immediate b))
    , (0x05, 1, \b _ -> Instruction ORA (ZeroPage b))
    , (0x15, 1, \b _ -> Instruction ORA (ZeroPageX b))
    , (0x0D, 2, \l h -> Instruction ORA (Absolute (w16 l h)))
    , (0x1D, 2, \l h -> Instruction ORA (AbsoluteX (w16 l h)))
    , (0x19, 2, \l h -> Instruction ORA (AbsoluteY (w16 l h)))
    , (0x01, 1, \b _ -> Instruction ORA (IndirectX b))
    , (0x11, 1, \b _ -> Instruction ORA (IndirectY b))
    -- PHA, PHP, PLA, PLP
    , (0x48, 0, \_ _ -> Instruction PHA Implied)
    , (0x08, 0, \_ _ -> Instruction PHP Implied)
    , (0x68, 0, \_ _ -> Instruction PLA Implied)
    , (0x28, 0, \_ _ -> Instruction PLP Implied)
    -- ROL
    , (0x2A, 0, \_ _ -> Instruction ROL Accumulator)
    , (0x26, 1, \b _ -> Instruction ROL (ZeroPage b))
    , (0x36, 1, \b _ -> Instruction ROL (ZeroPageX b))
    , (0x2E, 2, \l h -> Instruction ROL (Absolute (w16 l h)))
    , (0x3E, 2, \l h -> Instruction ROL (AbsoluteX (w16 l h)))
    -- ROR
    , (0x6A, 0, \_ _ -> Instruction ROR Accumulator)
    , (0x66, 1, \b _ -> Instruction ROR (ZeroPage b))
    , (0x76, 1, \b _ -> Instruction ROR (ZeroPageX b))
    , (0x6E, 2, \l h -> Instruction ROR (Absolute (w16 l h)))
    , (0x7E, 2, \l h -> Instruction ROR (AbsoluteX (w16 l h)))
    -- RTI, RTS
    , (0x40, 0, \_ _ -> Instruction RTI Implied)
    , (0x60, 0, \_ _ -> Instruction RTS Implied)
    -- SBC
    , (0xE9, 1, \b _ -> Instruction SBC (Immediate b))
    , (0xE5, 1, \b _ -> Instruction SBC (ZeroPage b))
    , (0xF5, 1, \b _ -> Instruction SBC (ZeroPageX b))
    , (0xED, 2, \l h -> Instruction SBC (Absolute (w16 l h)))
    , (0xFD, 2, \l h -> Instruction SBC (AbsoluteX (w16 l h)))
    , (0xF9, 2, \l h -> Instruction SBC (AbsoluteY (w16 l h)))
    , (0xE1, 1, \b _ -> Instruction SBC (IndirectX b))
    , (0xF1, 1, \b _ -> Instruction SBC (IndirectY b))
    -- SEC, SED, SEI
    , (0x38, 0, \_ _ -> Instruction SEC Implied)
    , (0xF8, 0, \_ _ -> Instruction SED Implied)
    , (0x78, 0, \_ _ -> Instruction SEI Implied)
    -- STA
    , (0x85, 1, \b _ -> Instruction STA (ZeroPage b))
    , (0x95, 1, \b _ -> Instruction STA (ZeroPageX b))
    , (0x8D, 2, \l h -> Instruction STA (Absolute (w16 l h)))
    , (0x9D, 2, \l h -> Instruction STA (AbsoluteX (w16 l h)))
    , (0x99, 2, \l h -> Instruction STA (AbsoluteY (w16 l h)))
    , (0x81, 1, \b _ -> Instruction STA (IndirectX b))
    , (0x91, 1, \b _ -> Instruction STA (IndirectY b))
    -- STX
    , (0x86, 1, \b _ -> Instruction STX (ZeroPage b))
    , (0x96, 1, \b _ -> Instruction STX (ZeroPageY b))
    , (0x8E, 2, \l h -> Instruction STX (Absolute (w16 l h)))
    -- STY
    , (0x84, 1, \b _ -> Instruction STY (ZeroPage b))
    , (0x94, 1, \b _ -> Instruction STY (ZeroPageX b))
    , (0x8C, 2, \l h -> Instruction STY (Absolute (w16 l h)))
    -- TAX, TAY, TSX, TXA, TXS, TYA
    , (0xAA, 0, \_ _ -> Instruction TAX Implied)
    , (0xA8, 0, \_ _ -> Instruction TAY Implied)
    , (0xBA, 0, \_ _ -> Instruction TSX Implied)
    , (0x8A, 0, \_ _ -> Instruction TXA Implied)
    , (0x9A, 0, \_ _ -> Instruction TXS Implied)
    , (0x98, 0, \_ _ -> Instruction TYA Implied)
    ]

w16 :: Word8 -> Word8 -> Word16
w16 l h = fromIntegral l + fromIntegral h * 256

decodeTable :: Array Word8 (Maybe (Int, Word8 -> Word8 -> Instruction))
decodeTable = listArray (0x00, 0xFF)
    [ lookup' i | i <- [0x00 .. 0xFF] ]
  where
    lookup' :: Word8 -> Maybe (Int, Word8 -> Word8 -> Instruction)
    lookup' i = case [ (n, f) | (op, n, f) <- decodeEntries, op == i ] of
        [(n, f)] -> Just (n, f)
        _        -> Nothing

-- ---------------------------------------------------------------------------
-- baseCycles: base cycle cost (no page-cross/branch-taken penalties)
-- ---------------------------------------------------------------------------

baseCycles :: Instruction -> Int
baseCycles (Instruction opc mode) = case (opc, mode) of
    -- ADC
    (ADC, Immediate{})  -> 2;  (ADC, ZeroPage{})  -> 3;  (ADC, ZeroPageX{}) -> 4
    (ADC, Absolute{})   -> 4;  (ADC, AbsoluteX{}) -> 4;  (ADC, AbsoluteY{}) -> 4
    (ADC, IndirectX{})  -> 6;  (ADC, IndirectY{})  -> 5
    -- AND
    (AND, Immediate{})  -> 2;  (AND, ZeroPage{})  -> 3;  (AND, ZeroPageX{}) -> 4
    (AND, Absolute{})   -> 4;  (AND, AbsoluteX{}) -> 4;  (AND, AbsoluteY{}) -> 4
    (AND, IndirectX{})  -> 6;  (AND, IndirectY{})  -> 5
    -- ASL
    (ASL, Accumulator)  -> 2;  (ASL, ZeroPage{})  -> 5;  (ASL, ZeroPageX{}) -> 6
    (ASL, Absolute{})   -> 6;  (ASL, AbsoluteX{}) -> 7
    -- Branches (base = 2, +1 if taken, +1 if page cross — not included here)
    (BCC, Relative{})   -> 2;  (BCS, Relative{})  -> 2
    (BEQ, Relative{})   -> 2;  (BMI, Relative{})  -> 2
    (BNE, Relative{})   -> 2;  (BPL, Relative{})  -> 2
    (BVC, Relative{})   -> 2;  (BVS, Relative{})  -> 2
    -- BIT
    (BIT, ZeroPage{})   -> 3;  (BIT, Absolute{})  -> 4
    -- BRK
    (BRK, Implied)      -> 7
    -- CLC, CLD, CLI, CLV
    (CLC, Implied)      -> 2;  (CLD, Implied)     -> 2
    (CLI, Implied)      -> 2;  (CLV, Implied)     -> 2
    -- CMP
    (CMP, Immediate{})  -> 2;  (CMP, ZeroPage{})  -> 3;  (CMP, ZeroPageX{}) -> 4
    (CMP, Absolute{})   -> 4;  (CMP, AbsoluteX{}) -> 4;  (CMP, AbsoluteY{}) -> 4
    (CMP, IndirectX{})  -> 6;  (CMP, IndirectY{})  -> 5
    -- CPX
    (CPX, Immediate{})  -> 2;  (CPX, ZeroPage{})  -> 3;  (CPX, Absolute{})  -> 4
    -- CPY
    (CPY, Immediate{})  -> 2;  (CPY, ZeroPage{})  -> 3;  (CPY, Absolute{})  -> 4
    -- DEC
    (DEC, ZeroPage{})   -> 5;  (DEC, ZeroPageX{}) -> 6
    (DEC, Absolute{})   -> 6;  (DEC, AbsoluteX{}) -> 7
    -- DEX, DEY
    (DEX, Implied)      -> 2;  (DEY, Implied)     -> 2
    -- EOR
    (EOR, Immediate{})  -> 2;  (EOR, ZeroPage{})  -> 3;  (EOR, ZeroPageX{}) -> 4
    (EOR, Absolute{})   -> 4;  (EOR, AbsoluteX{}) -> 4;  (EOR, AbsoluteY{}) -> 4
    (EOR, IndirectX{})  -> 6;  (EOR, IndirectY{})  -> 5
    -- INC
    (INC, ZeroPage{})   -> 5;  (INC, ZeroPageX{}) -> 6
    (INC, Absolute{})   -> 6;  (INC, AbsoluteX{}) -> 7
    -- INX, INY
    (INX, Implied)      -> 2;  (INY, Implied)     -> 2
    -- JMP
    (JMP, Absolute{})   -> 3;  (JMP, Indirect{})  -> 5
    -- JSR
    (JSR, Absolute{})   -> 6
    -- LDA
    (LDA, Immediate{})  -> 2;  (LDA, ZeroPage{})  -> 3;  (LDA, ZeroPageX{}) -> 4
    (LDA, Absolute{})   -> 4;  (LDA, AbsoluteX{}) -> 4;  (LDA, AbsoluteY{}) -> 4
    (LDA, IndirectX{})  -> 6;  (LDA, IndirectY{})  -> 5
    -- LDX
    (LDX, Immediate{})  -> 2;  (LDX, ZeroPage{})  -> 3;  (LDX, ZeroPageY{}) -> 4
    (LDX, Absolute{})   -> 4;  (LDX, AbsoluteY{}) -> 4
    -- LDY
    (LDY, Immediate{})  -> 2;  (LDY, ZeroPage{})  -> 3;  (LDY, ZeroPageX{}) -> 4
    (LDY, Absolute{})   -> 4;  (LDY, AbsoluteX{}) -> 4
    -- LSR
    (LSR, Accumulator)  -> 2;  (LSR, ZeroPage{})  -> 5;  (LSR, ZeroPageX{}) -> 6
    (LSR, Absolute{})   -> 6;  (LSR, AbsoluteX{}) -> 7
    -- NOP
    (NOP, Implied)      -> 2
    -- ORA
    (ORA, Immediate{})  -> 2;  (ORA, ZeroPage{})  -> 3;  (ORA, ZeroPageX{}) -> 4
    (ORA, Absolute{})   -> 4;  (ORA, AbsoluteX{}) -> 4;  (ORA, AbsoluteY{}) -> 4
    (ORA, IndirectX{})  -> 6;  (ORA, IndirectY{})  -> 5
    -- PHA, PHP, PLA, PLP
    (PHA, Implied)      -> 3;  (PHP, Implied)     -> 3
    (PLA, Implied)      -> 4;  (PLP, Implied)     -> 4
    -- ROL
    (ROL, Accumulator)  -> 2;  (ROL, ZeroPage{})  -> 5;  (ROL, ZeroPageX{}) -> 6
    (ROL, Absolute{})   -> 6;  (ROL, AbsoluteX{}) -> 7
    -- ROR
    (ROR, Accumulator)  -> 2;  (ROR, ZeroPage{})  -> 5;  (ROR, ZeroPageX{}) -> 6
    (ROR, Absolute{})   -> 6;  (ROR, AbsoluteX{}) -> 7
    -- RTI, RTS
    (RTI, Implied)      -> 6;  (RTS, Implied)     -> 6
    -- SBC
    (SBC, Immediate{})  -> 2;  (SBC, ZeroPage{})  -> 3;  (SBC, ZeroPageX{}) -> 4
    (SBC, Absolute{})   -> 4;  (SBC, AbsoluteX{}) -> 4;  (SBC, AbsoluteY{}) -> 4
    (SBC, IndirectX{})  -> 6;  (SBC, IndirectY{})  -> 5
    -- SEC, SED, SEI
    (SEC, Implied)      -> 2;  (SED, Implied)     -> 2;  (SEI, Implied)     -> 2
    -- STA
    (STA, ZeroPage{})   -> 3;  (STA, ZeroPageX{}) -> 4
    (STA, Absolute{})   -> 4;  (STA, AbsoluteX{}) -> 5;  (STA, AbsoluteY{}) -> 5
    (STA, IndirectX{})  -> 6;  (STA, IndirectY{})  -> 6
    -- STX
    (STX, ZeroPage{})   -> 3;  (STX, ZeroPageY{}) -> 4;  (STX, Absolute{})  -> 4
    -- STY
    (STY, ZeroPage{})   -> 3;  (STY, ZeroPageX{}) -> 4;  (STY, Absolute{})  -> 4
    -- TAX, TAY, TSX, TXA, TXS, TYA
    (TAX, Implied)      -> 2;  (TAY, Implied)     -> 2
    (TSX, Implied)      -> 2;  (TXA, Implied)     -> 2
    (TXS, Implied)      -> 2;  (TYA, Implied)     -> 2
    -- Invalid
    _ -> error $ "baseCycles: invalid " ++ show opc ++ " " ++ show mode

-- ---------------------------------------------------------------------------
-- canPageCross: True for read instructions with indexed addressing
-- ---------------------------------------------------------------------------

-- | True when the instruction can incur a +1 cycle page-crossing penalty.
-- This applies to read instructions using AbsoluteX, AbsoluteY, or IndirectY.
-- Write/RMW instructions (STA, INC, DEC, ASL, LSR, ROL, ROR) always pay the
-- extra cycle regardless, so their base cost already includes it.
canPageCross :: Instruction -> Bool
canPageCross (Instruction opc mode) = isRead opc && isIndexed mode
  where
    isRead op = op `elem` [ADC, AND, BIT, CMP, CPX, CPY, EOR, LDA, LDX, LDY, ORA, SBC]
    isIndexed AbsoluteX{} = True
    isIndexed AbsoluteY{} = True
    isIndexed IndirectY{}  = True
    isIndexed _            = False
