module Frontend.ACME.Parser (parseAcme) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Void (Void)
import Data.Word (Word16)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import ISA.Mos6502 (Opcode(..))
import Frontend.ACME.Syntax

type Parser = Parsec Void String

-- | Parse ACME assembly source into a list of AST lines.
parseAcme :: FilePath -> String -> Either (ParseErrorBundle String Void) [AsmLine]
parseAcme = parse asmFile

asmFile :: Parser [AsmLine]
asmFile = concat <$> many asmLine <* eof

asmLine :: Parser [AsmLine]
asmLine = do
    notFollowedBy eof
    hspace
    items <- fromMaybe [] <$> optional lineContent
    skipComment
    void eol <|> eof
    pure items

lineContent :: Parser [AsmLine]
lineContent = choice
    [ (:[]) <$> try originLine
    , (:[]) <$> try directive
    , skipPlusMacro >> pure []
    , labelAndInstr
    ]

-- ---------------------------------------------------------------------------
-- Origin: * = expr
-- ---------------------------------------------------------------------------

originLine :: Parser AsmLine
originLine = do
    _ <- char '*'
    hspace >> char '=' >> hspace
    LineOrigin <$> expr

-- ---------------------------------------------------------------------------
-- Directives: !byte, !word, !addr
-- ---------------------------------------------------------------------------

directive :: Parser AsmLine
directive = do
    _ <- char '!'
    choice
        [ string' "byte" >> hspace >> (LineData . DirByte) <$> exprList
        , string' "word" >> hspace >> (LineData . DirWord) <$> exprList
        , try (string' "addr") >> hspace1 >> equateBody
        , skipUnknownDirective >> pure LineBlank
        ]

equateBody :: Parser AsmLine
equateBody = do
    name <- identifier
    hspace >> char '=' >> hspace
    LineEquate name <$> expr

exprList :: Parser [Expr]
exprList = expr `sepBy1` (hspace >> char ',' >> hspace)

skipUnknownDirective :: Parser ()
skipUnknownDirective = void $ takeWhileP Nothing (\c -> c /= '\n' && c /= '\r' && c /= ';')

-- ---------------------------------------------------------------------------
-- Macro skip: +MacroCall
-- ---------------------------------------------------------------------------

skipPlusMacro :: Parser ()
skipPlusMacro = void $ do
    _ <- char '+'
    void $ takeWhileP Nothing (\c -> c /= '\n' && c /= '\r' && c /= ';')

-- ---------------------------------------------------------------------------
-- Label and/or instruction
-- ---------------------------------------------------------------------------

labelAndInstr :: Parser [AsmLine]
labelAndInstr = do
    mlabel <- optional (try labelDef)
    hspace
    minstr <- optional (try instrLine)
    let items = catMaybes [mlabel, minstr]
    if null items then empty else pure items

labelDef :: Parser AsmLine
labelDef = do
    name <- identifier
    _ <- char ':'
    pure (LineLabel name)

instrLine :: Parser AsmLine
instrLine = do
    opc <- mnemonic
    operand <- parseOperand opc
    pure (LineInstr opc operand)

-- ---------------------------------------------------------------------------
-- Mnemonic
-- ---------------------------------------------------------------------------

mnemonic :: Parser Opcode
mnemonic = try $ do
    name <- takeWhile1P (Just "letter") isAlpha
    notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_'))
    case Map.lookup (map toLower name) opcodeMap of
        Just opc -> pure opc
        Nothing  -> fail $ "unknown mnemonic: " ++ name

opcodeMap :: Map.Map String Opcode
opcodeMap = Map.fromList [(map toLower (show opc), opc) | opc <- [minBound..maxBound]]

-- ---------------------------------------------------------------------------
-- Operand parsing
-- ---------------------------------------------------------------------------

parseOperand :: Opcode -> Parser SymOperand
parseOperand opc = do
    hspace
    mc <- optional (lookAhead anySingle)
    case mc of
        Nothing  -> pure (defaultNoOperand opc)
        Just ';' -> pure (defaultNoOperand opc)
        Just '\n' -> pure (defaultNoOperand opc)
        Just '\r' -> pure (defaultNoOperand opc)
        Just '#'  -> anySingle >> parseImmediate
        Just '('  -> anySingle >> parseIndirectForm
        _         -> parseAddressForm opc

defaultNoOperand :: Opcode -> SymOperand
defaultNoOperand opc
    | opc `elem` [ASL, LSR, ROL, ROR] = SAccumulator
    | otherwise = SImplied

parseImmediate :: Parser SymOperand
parseImmediate = choice
    [ char '<' >> SImmLo <$> expr
    , char '>' >> SImmHi <$> expr
    , SImmediate <$> expr
    ]

parseIndirectForm :: Parser SymOperand
parseIndirectForm = do
    hspace
    e <- expr
    hspace
    choice
        [ do _ <- char ','; hspace; _ <- char' 'x'; hspace; _ <- char ')'; pure (SIndirectX e)
        , do _ <- char ')'; afterCloseParen e
        ]
  where
    afterCloseParen e = do
        hspace
        choice
            [ do _ <- char ','; hspace; _ <- char' 'y'; pure (SIndirectY e)
            , pure (SIndirect e)
            ]

parseAddressForm :: Opcode -> Parser SymOperand
parseAddressForm opc
    | opc `elem` [ASL, LSR, ROL, ROR] =
        try (do _ <- char' 'a'; notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_')); pure SAccumulator)
        <|> parseAddressWithIndex
    | otherwise = parseAddressWithIndex

parseAddressWithIndex :: Parser SymOperand
parseAddressWithIndex = do
    e <- expr
    hspace
    choice
        [ do _ <- char ','
             hspace
             choice [ char' 'x' >> pure (SAddressX e)
                    , char' 'y' >> pure (SAddressY e)
                    ]
        , pure (SAddress e)
        ]

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

expr :: Parser Expr
expr = do
    left <- term
    rest left
  where
    rest e = addOp e <|> subOp e <|> pure e
    addOp e = do hspace; _ <- char '+'; hspace; t <- term; rest (Add e t)
    subOp e = do hspace; _ <- char '-'; hspace; t <- term; rest (Sub e t)

term :: Parser Expr
term = choice
    [ char '<' >> Lo <$> atom
    , char '>' >> Hi <$> atom
    , atom
    ]

atom :: Parser Expr
atom = Lit <$> number <|> Sym <$> identifier

-- ---------------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------------

number :: Parser Word16
number = choice
    [ char '$' *> L.hexadecimal
    , char '%' *> L.binary
    , try (string "0b" <|> string "0B") *> L.binary
    , L.decimal
    ]

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

identifier :: Parser String
identifier = do
    c0 <- satisfy (\c -> isAlphaNum c && not (isDigit c) || c == '_' || c == '.')
    cs <- takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
    pure (c0 : cs)
  where isDigit c = c >= '0' && c <= '9'

-- ---------------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------------

skipComment :: Parser ()
skipComment = do
    hspace
    _ <- optional (char ';' >> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r'))
    pure ()
