{-# LANGUAGE RecursiveDo #-}

module Frontend.ACME.Assemble (assembleAcme) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word16)

import Asm.Monad (MonadASM(..), Label(..), namedLabel)
import ISA.Mos6502 (Opcode(..), AddressingMode(..), Instruction(..), lo, hi)
import Frontend.ACME.Syntax

-- | Assemble parsed ACME lines into a MonadASM action.
--
-- The @equates@ map provides externally defined symbols (e.g. hardware
-- addresses).  Labels defined in the source are resolved via MonadFix
-- (forward references work).
assembleAcme :: MonadASM m => Map String Word16 -> [AsmLine] -> m ()
assembleAcme externalEquates lines_ = mdo
    let allEquates = extractEquates lines_ `Map.union` externalEquates
        resolve name = case Map.lookup name allEquates of
            Just addr -> addr
            Nothing   -> case Map.lookup name labelMap of
                Just lbl -> labelAddr lbl
                Nothing  -> error $ "assembleAcme: undefined symbol: " ++ name
    labelMap <- foldLines resolve Map.empty lines_
    pure ()

-- | Extract literal equates from source lines.
extractEquates :: [AsmLine] -> Map String Word16
extractEquates = Map.fromList . mapMaybe go
  where
    go (LineEquate name e) = Just (name, evalLit e)
    go _                   = Nothing
    evalLit (Lit w)   = w
    evalLit (Add a b) = evalLit a + evalLit b
    evalLit (Sub a b) = evalLit a - evalLit b
    evalLit (Lo e)    = fromIntegral (lo (evalLit e))
    evalLit (Hi e)    = fromIntegral (hi (evalLit e))
    evalLit (Sym s)   = error $ "assembleAcme: non-literal equate references symbol: " ++ s

-- | Process lines sequentially, accumulating a label map.
foldLines :: MonadASM m
          => (String -> Word16) -> Map String Label -> [AsmLine] -> m (Map String Label)
foldLines _ labelMap [] = pure labelMap
foldLines resolve labelMap (line_ : rest) = do
    labelMap' <- processLine resolve labelMap line_
    foldLines resolve labelMap' rest

processLine :: MonadASM m
            => (String -> Word16) -> Map String Label -> AsmLine -> m (Map String Label)
processLine _ labelMap (LineLabel name) = do
    lbl <- namedLabel name
    pure (Map.insert name lbl labelMap)
processLine resolve labelMap (LineInstr opc operand) = do
    emitResolvedInstr resolve opc operand
    pure labelMap
processLine resolve labelMap (LineData dir) = do
    emitData resolve dir
    pure labelMap
processLine _ labelMap LineBlank      = pure labelMap
processLine _ labelMap (LineOrigin _) = pure labelMap
processLine _ labelMap (LineEquate _ _) = pure labelMap

-- ---------------------------------------------------------------------------
-- Instruction emission
-- ---------------------------------------------------------------------------

emitResolvedInstr :: MonadASM m => (String -> Word16) -> Opcode -> SymOperand -> m ()
emitResolvedInstr resolve opc operand
    | isBranch opc = case operand of
        SAddress e -> emitBranch' opc (resolveExpr resolve e)
        _          -> error $ "invalid operand for branch: " ++ show opc
    | otherwise = emitInstruction (Instruction opc (resolveOperand resolve operand))

emitBranch' :: MonadASM m => Opcode -> Word16 -> m ()
emitBranch' opc target = do
    pc <- currentPC
    let diff = fromIntegral target - fromIntegral pc - 2 :: Int
        offset
            | diff < -128 || diff > 127 = error $ show opc ++ " branch out of range: " ++ show diff
            | otherwise = fromIntegral diff
    emitInstruction (Instruction opc (Relative offset))

-- ---------------------------------------------------------------------------
-- Operand resolution
-- ---------------------------------------------------------------------------

resolveOperand :: (String -> Word16) -> SymOperand -> AddressingMode
resolveOperand _ SImplied       = Implied
resolveOperand _ SAccumulator   = Accumulator
resolveOperand r (SImmediate e) = Immediate (fromIntegral (resolveExpr r e))
resolveOperand r (SImmLo e)     = Immediate (lo (resolveExpr r e))
resolveOperand r (SImmHi e)     = Immediate (hi (resolveExpr r e))
resolveOperand r (SAddress e)   = resolveAddress r e
resolveOperand r (SAddressX e)  = resolveAddressX r e
resolveOperand r (SAddressY e)  = resolveAddressY r e
resolveOperand r (SIndirect e)  = Indirect (resolveExpr r e)
resolveOperand r (SIndirectX e) = IndirectX (fromIntegral (resolveExpr r e))
resolveOperand r (SIndirectY e) = IndirectY (fromIntegral (resolveExpr r e))

resolveAddress :: (String -> Word16) -> Expr -> AddressingMode
resolveAddress r e = case isLiteral e of
    Just val | val <= 0xFF -> ZeroPage (fromIntegral val)
    _                      -> Absolute (resolveExpr r e)

resolveAddressX :: (String -> Word16) -> Expr -> AddressingMode
resolveAddressX r e = case isLiteral e of
    Just val | val <= 0xFF -> ZeroPageX (fromIntegral val)
    _                      -> AbsoluteX (resolveExpr r e)

resolveAddressY :: (String -> Word16) -> Expr -> AddressingMode
resolveAddressY r e = case isLiteral e of
    Just val | val <= 0xFF -> ZeroPageY (fromIntegral val)
    _                      -> AbsoluteY (resolveExpr r e)

-- ---------------------------------------------------------------------------
-- Expression evaluation
-- ---------------------------------------------------------------------------

resolveExpr :: (String -> Word16) -> Expr -> Word16
resolveExpr _ (Lit w)   = w
resolveExpr r (Sym s)   = r s
resolveExpr r (Lo e)    = fromIntegral (lo (resolveExpr r e))
resolveExpr r (Hi e)    = fromIntegral (hi (resolveExpr r e))
resolveExpr r (Add a b) = resolveExpr r a + resolveExpr r b
resolveExpr r (Sub a b) = resolveExpr r a - resolveExpr r b

-- | Try to evaluate an expression using only literals (no symbol lookups).
isLiteral :: Expr -> Maybe Word16
isLiteral (Lit w)   = Just w
isLiteral (Add a b) = (+) <$> isLiteral a <*> isLiteral b
isLiteral (Sub a b) = (-) <$> isLiteral a <*> isLiteral b
isLiteral (Lo e)    = (fromIntegral . lo) <$> isLiteral e
isLiteral (Hi e)    = (fromIntegral . hi) <$> isLiteral e
isLiteral (Sym _)   = Nothing

-- ---------------------------------------------------------------------------
-- Data emission
-- ---------------------------------------------------------------------------

emitData :: MonadASM m => (String -> Word16) -> DataDir -> m ()
emitData resolve (DirByte exprs) =
    emitBytes (map (fromIntegral . resolveExpr resolve) exprs)
emitData resolve (DirWord exprs) =
    emitBytes (concatMap (\e -> let w = resolveExpr resolve e in [lo w, hi w]) exprs)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isBranch :: Opcode -> Bool
isBranch opc = opc `elem` [BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS]
