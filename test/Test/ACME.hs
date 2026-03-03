{-# LANGUAGE RecursiveDo #-}

module Test.ACME (tests) where

import Data.Map.Strict qualified as Map
import Data.Word (Word8, Word16)

import Asm.Monad (MonadASM(..), assemble, namedLabel)
import Asm.Mos6502
import Backend.ACME (AcmeASM, exportAcme)
import Frontend.ACME.Syntax
import Frontend.ACME.Parser (parseAcme)
import Frontend.ACME.Assemble (assembleAcme)
import ISA.Mos6502 (Opcode(..))
import Test.Helpers

-- ---------------------------------------------------------------------------
-- Parser tests
-- ---------------------------------------------------------------------------

parseOk :: String -> [AsmLine]
parseOk src = case parseAcme "<test>" src of
    Right ls -> ls
    Left  e  -> error (show e)

prop_parseImplied :: Bool
prop_parseImplied =
    parseOk "    nop\n" == [LineInstr NOP SImplied]

prop_parseImmediate :: Bool
prop_parseImmediate =
    parseOk "    lda #$42\n" == [LineInstr LDA (SImmediate (Lit 0x42))]

prop_parseImmLo :: Bool
prop_parseImmLo =
    parseOk "    lda #<$1234\n" == [LineInstr LDA (SImmLo (Lit 0x1234))]

prop_parseImmHi :: Bool
prop_parseImmHi =
    parseOk "    lda #>$1234\n" == [LineInstr LDA (SImmHi (Lit 0x1234))]

prop_parseZeroPage :: Bool
prop_parseZeroPage =
    parseOk "    lda $42\n" == [LineInstr LDA (SAddress (Lit 0x42))]

prop_parseAbsolute :: Bool
prop_parseAbsolute =
    parseOk "    lda $1234\n" == [LineInstr LDA (SAddress (Lit 0x1234))]

prop_parseAddressX :: Bool
prop_parseAddressX =
    parseOk "    lda $1234,x\n" == [LineInstr LDA (SAddressX (Lit 0x1234))]

prop_parseAddressY :: Bool
prop_parseAddressY =
    parseOk "    lda $1234,y\n" == [LineInstr LDA (SAddressY (Lit 0x1234))]

prop_parseIndirectX :: Bool
prop_parseIndirectX =
    parseOk "    lda ($42,x)\n" == [LineInstr LDA (SIndirectX (Lit 0x42))]

prop_parseIndirectY :: Bool
prop_parseIndirectY =
    parseOk "    lda ($42),y\n" == [LineInstr LDA (SIndirectY (Lit 0x42))]

prop_parseIndirect :: Bool
prop_parseIndirect =
    parseOk "    jmp ($1234)\n" == [LineInstr JMP (SIndirect (Lit 0x1234))]

prop_parseLabel :: Bool
prop_parseLabel =
    parseOk "loop:\n" == [LineLabel "loop"]

prop_parseLabelAndInstr :: Bool
prop_parseLabelAndInstr =
    parseOk "loop: nop\n" == [LineLabel "loop", LineInstr NOP SImplied]

prop_parseOrigin :: Bool
prop_parseOrigin =
    parseOk "* = $c000\n" == [LineOrigin (Lit 0xC000)]

prop_parseByte :: Bool
prop_parseByte =
    parseOk "    !byte $42, $43\n" == [LineData (DirByte [Lit 0x42, Lit 0x43])]

prop_parseWord :: Bool
prop_parseWord =
    parseOk "    !word $1234\n" == [LineData (DirWord [Lit 0x1234])]

prop_parseSymbol :: Bool
prop_parseSymbol =
    parseOk "    lda label\n" == [LineInstr LDA (SAddress (Sym "label"))]

prop_parseExprAdd :: Bool
prop_parseExprAdd =
    parseOk "    lda label + 1\n" == [LineInstr LDA (SAddress (Add (Sym "label") (Lit 1)))]

prop_parseBinary :: Bool
prop_parseBinary =
    parseOk "    lda #%11001010\n" == [LineInstr LDA (SImmediate (Lit 0xCA))]

prop_parseComment :: Bool
prop_parseComment =
    parseOk "    nop ; do nothing\n" == [LineInstr NOP SImplied]

prop_parseBlankLine :: Bool
prop_parseBlankLine =
    parseOk "\n" == []

prop_parseCaseInsensitive :: Bool
prop_parseCaseInsensitive =
    parseOk "    LDA #$42\n" == [LineInstr LDA (SImmediate (Lit 0x42))]

prop_parseAccumulator :: Bool
prop_parseAccumulator =
    parseOk "    asl\n" == [LineInstr ASL SAccumulator]

prop_parseEquate :: Bool
prop_parseEquate =
    parseOk "    !addr screen = $0400\n" == [LineEquate "screen" (Lit 0x0400)]

-- ---------------------------------------------------------------------------
-- Lowering tests
-- ---------------------------------------------------------------------------

prop_lowerSimple :: Bool
prop_lowerSimple =
    let lines_ = [LineInstr LDA (SImmediate (Lit 0x42)), LineInstr RTS SImplied]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xA9, 0x42, 0x60]

prop_lowerZeroPage :: Bool
prop_lowerZeroPage =
    let lines_ = [LineInstr LDA (SAddress (Lit 0x42))]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xA5, 0x42]

prop_lowerAbsolute :: Bool
prop_lowerAbsolute =
    let lines_ = [LineInstr LDA (SAddress (Lit 0x1234))]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xAD, 0x34, 0x12]

prop_lowerBackwardBranch :: Bool
prop_lowerBackwardBranch =
    let lines_ = [ LineLabel "loop"
                  , LineInstr DEX SImplied
                  , LineInstr BNE (SAddress (Sym "loop"))
                  ]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xCA, 0xD0, 0xFD]

prop_lowerForwardBranch :: Bool
prop_lowerForwardBranch =
    let lines_ = [ LineInstr BEQ (SAddress (Sym "skip"))
                  , LineInstr NOP SImplied
                  , LineLabel "skip"
                  ]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xF0, 0x01, 0xEA]

prop_lowerEquate :: Bool
prop_lowerEquate =
    let lines_ = [ LineEquate "border" (Lit 0xD020)
                  , LineInstr LDA (SImmediate (Lit 0x00))
                  , LineInstr STA (SAddress (Sym "border"))
                  ]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xA9, 0x00, 0x8D, 0x20, 0xD0]

prop_lowerExternalEquate :: Bool
prop_lowerExternalEquate =
    let equates = Map.fromList [("VIC_BORDER", 0xD020)]
        lines_ = [LineInstr STA (SAddress (Sym "VIC_BORDER"))]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme equates lines_)
    in  bytes == [0x8D, 0x20, 0xD0]

prop_lowerImmLoHi :: Bool
prop_lowerImmLoHi =
    let lines_ = [ LineInstr LDA (SImmLo (Lit 0x1234))
                  , LineInstr LDX (SImmHi (Lit 0x1234))
                  ]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0xA9, 0x34, 0xA2, 0x12]

prop_lowerByteData :: Bool
prop_lowerByteData =
    let lines_ = [LineData (DirByte [Lit 0x42, Lit 0x43, Lit 0x44])]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0x42, 0x43, 0x44]

prop_lowerWordData :: Bool
prop_lowerWordData =
    let lines_ = [LineData (DirWord [Lit 0x1234, Lit 0x5678])]
        (_, bytes) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty lines_)
    in  bytes == [0x34, 0x12, 0x78, 0x56]

-- ---------------------------------------------------------------------------
-- Export tests
-- ---------------------------------------------------------------------------

prop_exportImplied :: Bool
prop_exportImplied =
    let src = exportAcme 0xC000 (nop :: AcmeASM ())
    in  src == "* = $c000\n    nop\n"

prop_exportImmediate :: Bool
prop_exportImmediate =
    let src = exportAcme 0xC000 (lda # 0x42 :: AcmeASM ())
    in  src == "* = $c000\n    lda #$42\n"

prop_exportAbsolute :: Bool
prop_exportAbsolute =
    let src = exportAcme 0xC000 (sta (0xD020 :: Word16) :: AcmeASM ())
    in  src == "* = $c000\n    sta $d020\n"

prop_exportZeroPage :: Bool
prop_exportZeroPage =
    let src = exportAcme 0xC000 (lda (0x42 :: Word8) :: AcmeASM ())
    in  src == "* = $c000\n    lda $42\n"

prop_exportIndexed :: Bool
prop_exportIndexed =
    let src = exportAcme 0xC000 (lda (0xD000 :: Word16, X) :: AcmeASM ())
    in  src == "* = $c000\n    lda $d000,x\n"

prop_exportIndirectY :: Bool
prop_exportIndirectY =
    let src = exportAcme 0xC000 (lda (IndY 0x42) :: AcmeASM ())
    in  src == "* = $c000\n    lda ($42),y\n"

prop_exportNamedLabel :: Bool
prop_exportNamedLabel =
    let prog :: AcmeASM ()
        prog = do
            _ <- namedLabel "start"
            lda # 0x42
            rts
        src = exportAcme 0xC000 prog
    in  src == "* = $c000\nstart:\n    lda #$42\n    rts\n"

prop_exportBranchWithLabel :: Bool
prop_exportBranchWithLabel =
    let prog :: AcmeASM ()
        prog = do
            loop <- namedLabel "loop"
            dex
            bne loop
        src = exportAcme 0xC000 prog
    in  src == "* = $c000\nloop:\n    dex\n    bne loop\n"

prop_exportByteData :: Bool
prop_exportByteData =
    let src = exportAcme 0xC000 (emitBytes [0x42, 0x43] :: AcmeASM ())
    in  src == "* = $c000\n    !byte $42, $43\n"

-- ---------------------------------------------------------------------------
-- Roundtrip tests
-- ---------------------------------------------------------------------------

prop_roundtripSimple :: Bool
prop_roundtripSimple =
    let prog :: MonadASM m => m ()
        prog = do
            _ <- namedLabel "start"
            lda # 0x42
            sta (0xD020 :: Word16)
            rts

        (_, bytes1) = assemble (simpleConfig 0xC000) prog

        acmeText = exportAcme 0xC000 prog
        parsed = unsafeParse acmeText
        (_, bytes2) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty parsed)
    in  bytes1 == bytes2

prop_roundtripBranch :: Bool
prop_roundtripBranch =
    let prog :: MonadASM m => m ()
        prog = do
            loop <- namedLabel "loop"
            dex
            bne loop

        (_, bytes1) = assemble (simpleConfig 0xC000) prog

        acmeText = exportAcme 0xC000 prog
        parsed = unsafeParse acmeText
        (_, bytes2) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty parsed)
    in  bytes1 == bytes2

prop_roundtripForwardRef :: Bool
prop_roundtripForwardRef =
    let prog :: MonadASM m => m ()
        prog = mdo
            beq skip
            nop
            skip <- namedLabel "skip"
            rts

        (_, bytes1) = assemble (simpleConfig 0xC000) prog

        acmeText = exportAcme 0xC000 prog
        parsed = unsafeParse acmeText
        (_, bytes2) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty parsed)
    in  bytes1 == bytes2

prop_roundtripData :: Bool
prop_roundtripData =
    let prog :: MonadASM m => m ()
        prog = do
            _ <- namedLabel "data"
            emitBytes [0x01, 0x02, 0x03]

        (_, bytes1) = assemble (simpleConfig 0xC000) prog

        acmeText = exportAcme 0xC000 prog
        parsed = unsafeParse acmeText
        (_, bytes2) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty parsed)
    in  bytes1 == bytes2

prop_roundtripMultiInstr :: Bool
prop_roundtripMultiInstr =
    let prog :: MonadASM m => m ()
        prog = do
            _ <- namedLabel "init"
            sei
            cld
            ldx # 0xFF
            txs
            lda # 0x00
            sta (0xD020 :: Word16)
            sta (0xD021 :: Word16)
            rts

        (_, bytes1) = assemble (simpleConfig 0xC000) prog

        acmeText = exportAcme 0xC000 prog
        parsed = unsafeParse acmeText
        (_, bytes2) = assemble (simpleConfig 0xC000) (assembleAcme Map.empty parsed)
    in  bytes1 == bytes2

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse ACME text or error.
unsafeParse :: String -> [AsmLine]
unsafeParse src = case parseAcme "<test>" src of
    Right ls -> ls
    Left e   -> error (show e)

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

tests :: [IO Bool]
tests =
    [ section "ACME parser"
    , checkOnce "parse implied (nop)"        prop_parseImplied
    , checkOnce "parse immediate (#$42)"     prop_parseImmediate
    , checkOnce "parse immediate lo (#<)"    prop_parseImmLo
    , checkOnce "parse immediate hi (#>)"    prop_parseImmHi
    , checkOnce "parse zero-page ($42)"      prop_parseZeroPage
    , checkOnce "parse absolute ($1234)"     prop_parseAbsolute
    , checkOnce "parse address,x"            prop_parseAddressX
    , checkOnce "parse address,y"            prop_parseAddressY
    , checkOnce "parse (zp,x)"              prop_parseIndirectX
    , checkOnce "parse (zp),y"              prop_parseIndirectY
    , checkOnce "parse (addr) indirect"      prop_parseIndirect
    , checkOnce "parse label"                prop_parseLabel
    , checkOnce "parse label + instr"        prop_parseLabelAndInstr
    , checkOnce "parse origin"               prop_parseOrigin
    , checkOnce "parse !byte"                prop_parseByte
    , checkOnce "parse !word"                prop_parseWord
    , checkOnce "parse symbol operand"       prop_parseSymbol
    , checkOnce "parse expr addition"        prop_parseExprAdd
    , checkOnce "parse binary literal"       prop_parseBinary
    , checkOnce "parse comment stripped"      prop_parseComment
    , checkOnce "parse blank line"           prop_parseBlankLine
    , checkOnce "parse case insensitive"     prop_parseCaseInsensitive
    , checkOnce "parse accumulator (asl)"    prop_parseAccumulator
    , checkOnce "parse equate (!addr)"       prop_parseEquate

    , section "ACME lowering"
    , checkOnce "lower simple instrs"        prop_lowerSimple
    , checkOnce "lower zero-page"            prop_lowerZeroPage
    , checkOnce "lower absolute"             prop_lowerAbsolute
    , checkOnce "lower backward branch"      prop_lowerBackwardBranch
    , checkOnce "lower forward branch"       prop_lowerForwardBranch
    , checkOnce "lower equate"               prop_lowerEquate
    , checkOnce "lower external equate"      prop_lowerExternalEquate
    , checkOnce "lower #< and #>"            prop_lowerImmLoHi
    , checkOnce "lower !byte data"           prop_lowerByteData
    , checkOnce "lower !word data"           prop_lowerWordData

    , section "ACME export"
    , checkOnce "export implied"             prop_exportImplied
    , checkOnce "export immediate"           prop_exportImmediate
    , checkOnce "export absolute"            prop_exportAbsolute
    , checkOnce "export zero-page"           prop_exportZeroPage
    , checkOnce "export indexed"             prop_exportIndexed
    , checkOnce "export (zp),y"              prop_exportIndirectY
    , checkOnce "export named label"         prop_exportNamedLabel
    , checkOnce "export branch with label"   prop_exportBranchWithLabel
    , checkOnce "export !byte data"          prop_exportByteData

    , section "ACME roundtrip"
    , checkOnce "roundtrip simple"           prop_roundtripSimple
    , checkOnce "roundtrip branch"           prop_roundtripBranch
    , checkOnce "roundtrip forward ref"      prop_roundtripForwardRef
    , checkOnce "roundtrip data"             prop_roundtripData
    , checkOnce "roundtrip multi-instr"      prop_roundtripMultiInstr
    ]
