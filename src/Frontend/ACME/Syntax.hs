module Frontend.ACME.Syntax
    ( AsmLine(..)
    , SymOperand(..)
    , Expr(..)
    , DataDir(..)
    ) where

import Data.Word (Word16)

import ISA.Mos6502 (Opcode)

data AsmLine
    = LineLabel   String
    | LineInstr   Opcode SymOperand
    | LineOrigin  Expr
    | LineData    DataDir
    | LineEquate  String Expr
    | LineBlank
    deriving (Show, Eq)

data SymOperand
    = SImplied
    | SAccumulator
    | SImmediate Expr
    | SImmLo Expr
    | SImmHi Expr
    | SAddress Expr
    | SAddressX Expr
    | SAddressY Expr
    | SIndirect Expr
    | SIndirectX Expr
    | SIndirectY Expr
    deriving (Show, Eq)

data Expr
    = Lit Word16
    | Sym String
    | Lo Expr
    | Hi Expr
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Show, Eq)

data DataDir
    = DirByte [Expr]
    | DirWord [Expr]
    deriving (Show, Eq)
