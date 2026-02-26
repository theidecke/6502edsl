module ISA.Mos6502
    ( Opcode(..)
    , AddressingMode(..)
    , Instruction(..)
    ) where

import Data.Int  (Int8)
import Data.Word (Word8, Word16)

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
