module Backend.ACME (AcmeASM, exportAcme, exportAcmeWith) where

import Control.Monad.Fix (MonadFix(..))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8, Word16)
import Numeric (showHex)

import Asm.Monad (MonadASM(..), MonadZPAlloc(..), TargetConfig(..), Label(..), findContiguous)
import ISA.Mos6502 (Instruction(..), AddressingMode(..), instrSize)

-- | Items recorded during AcmeASM execution.
data AcmeItem
    = AcmeInstr Word16 Instruction
    | AcmeBytes Word16 [Word8]

data AcmeState = AcmeState
    { acmePC          :: !Word16
    , acmeFreeZP      :: !(Set Word8)
    , acmeLabels      :: [Label]
    , acmeAnnotations :: [String]
    }

newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where Endo f <> Endo g = Endo (f . g)
instance Monoid (Endo a) where mempty = Endo id

-- | Assembly monad that records structured instructions for ACME text export.
newtype AcmeASM a = AcmeASM (AcmeState -> (a, AcmeState, Endo [AcmeItem]))

instance Functor AcmeASM where
    fmap f (AcmeASM g) = AcmeASM $ \s ->
        let (a, s', w) = g s in (f a, s', w)

instance Applicative AcmeASM where
    pure a = AcmeASM $ \s -> (a, s, mempty)
    AcmeASM f <*> AcmeASM g = AcmeASM $ \s ->
        let (fab, s1, w1) = f s
            (a,   s2, w2) = g s1
        in  (fab a, s2, w1 <> w2)

instance Monad AcmeASM where
    AcmeASM m >>= k = AcmeASM $ \s ->
        let (a,  s1, w1) = m s
            AcmeASM n     = k a
            (b,  s2, w2) = n s1
        in  (b, s2, w1 <> w2)

instance MonadFix AcmeASM where
    mfix f = AcmeASM $ \s ->
        let (a, s', w) = let AcmeASM g = f a in g s
        in  (a, s', w)

instance MonadASM AcmeASM where
    emitInstruction instr = AcmeASM $ \s ->
        let pc = acmePC s
        in  ((), s { acmePC = pc + fromIntegral (instrSize instr) }
            , Endo (AcmeInstr pc instr :))

    emitBytes bs = AcmeASM $ \s ->
        let pc = acmePC s
        in  ((), s { acmePC = pc + fromIntegral (length bs) }
            , Endo (AcmeBytes pc bs :))

    currentPC = AcmeASM $ \s -> (acmePC s, s, mempty)

    registerLabel lbl = AcmeASM $ \s ->
        ((), s { acmeLabels = lbl : acmeLabels s }, mempty)

    pushAnnotation name = AcmeASM $ \s ->
        ((), s { acmeAnnotations = name : acmeAnnotations s }, mempty)

    popAnnotation = AcmeASM $ \s ->
        ((), s { acmeAnnotations = drop 1 (acmeAnnotations s) }, mempty)

instance MonadZPAlloc AcmeASM where
    allocZP n
        | n <= 0    = error "allocZP: requested size must be positive"
        | otherwise = AcmeASM $ \s ->
            let free  = acmeFreeZP s
                start = findContiguous n (Set.toAscList free)
                block = Set.fromList [start .. start + fromIntegral n - 1]
            in  (start, s { acmeFreeZP = free `Set.difference` block }, mempty)

-- ---------------------------------------------------------------------------
-- Export
-- ---------------------------------------------------------------------------

-- | Run an AcmeASM program and produce ACME assembly text.
-- Uses an empty zero-page pool (no ZP allocation available).
exportAcme :: Word16 -> AcmeASM a -> String
exportAcme org (AcmeASM f) =
    let s0 = AcmeState { acmePC = org, acmeFreeZP = Set.empty
                        , acmeLabels = [], acmeAnnotations = [] }
        (_a, s, Endo dl) = f s0
        items = dl []
        addrToName = buildLabelMap (acmeLabels s)
    in  renderAcme org items addrToName

-- | Run an AcmeASM program using a 'TargetConfig' for origin and ZP pool.
exportAcmeWith :: TargetConfig -> AcmeASM a -> String
exportAcmeWith cfg (AcmeASM f) =
    let org = origin cfg
        s0 = AcmeState { acmePC = org, acmeFreeZP = freeZeroPage cfg
                        , acmeLabels = [], acmeAnnotations = [] }
        (_a, s, Endo dl) = f s0
        items = dl []
        addrToName = buildLabelMap (acmeLabels s)
    in  renderAcme org items addrToName

buildLabelMap :: [Label] -> Map Word16 String
buildLabelMap = Map.fromList . mapMaybe toEntry
  where
    toEntry (Label addr (Just name)) = Just (addr, name)
    toEntry _                        = Nothing

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderAcme :: Word16 -> [AcmeItem] -> Map Word16 String -> String
renderAcme org items labelMap =
    "* = $" ++ showHex16 org ++ "\n"
    ++ concatMap renderItem items
  where
    renderItem (AcmeInstr addr instr) =
        labelPrefix addr ++ "    " ++ renderInstr addr instr ++ "\n"
    renderItem (AcmeBytes _ []) = ""
    renderItem (AcmeBytes addr bs) =
        labelPrefix addr ++ "    !byte " ++ renderByteList bs ++ "\n"

    labelPrefix addr = case Map.lookup addr labelMap of
        Just name -> name ++ ":\n"
        Nothing   -> ""

    renderInstr addr (Instruction opc mode) =
        map toLower (show opc) ++ renderOperand addr mode

    renderOperand _ Implied          = ""
    renderOperand _ Accumulator      = ""
    renderOperand _ (Immediate v)    = " #$" ++ showHex8 v
    renderOperand _ (ZeroPage a)     = " $" ++ showHex8 a
    renderOperand _ (ZeroPageX a)    = " $" ++ showHex8 a ++ ",x"
    renderOperand _ (ZeroPageY a)    = " $" ++ showHex8 a ++ ",y"
    renderOperand _ (Absolute a)     = " " ++ addrStr a
    renderOperand _ (AbsoluteX a)    = " " ++ addrStr a ++ ",x"
    renderOperand _ (AbsoluteY a)    = " " ++ addrStr a ++ ",y"
    renderOperand _ (Indirect a)     = " (" ++ addrStr a ++ ")"
    renderOperand _ (IndirectX a)    = " ($" ++ showHex8 a ++ ",x)"
    renderOperand _ (IndirectY a)    = " ($" ++ showHex8 a ++ "),y"
    renderOperand addr (Relative r)  =
        let target = fromIntegral (fromIntegral addr + 2 + fromIntegral r :: Int) :: Word16
        in  " " ++ addrStr target

    addrStr a = case Map.lookup a labelMap of
        Just name -> name
        Nothing   -> "$" ++ showHex16 a

    renderByteList bs = intercalate ", " (map (\b -> "$" ++ showHex8 b) bs)

showHex8 :: Word8 -> String
showHex8 w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

showHex16 :: Word16 -> String
showHex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s
