module Emu.CPU
    ( CPUState(..), initCPU
    , Lens', view, set, over
    , regA, regX, regY, regSP, regP, regPC, mem, cycles
    , flagC, flagZ, flagI, flagD, flagB, flagV, flagN
    , memAt, updateNZ
    ) where

import Data.Bits ((.&.), (.|.), complement, testBit, shiftL)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8, Word16)

import Emu.Mem (Mem, emptyMem, readByte, writeByte)

-- | 6502 CPU state. Registers are strict; memory is lazy for future trace diffs.
data CPUState = CPUState
    { cpuA      :: !Word8
    , cpuX      :: !Word8
    , cpuY      :: !Word8
    , cpuSP     :: !Word8
    , cpuP      :: !Word8
    , cpuPC     :: !Word16
    , cpuMem    :: Mem
    , cpuCycles :: !Int
    } deriving (Show, Eq)

-- | Initial CPU state: A=0, X=0, Y=0, SP=0xFD, P=0x24 (I+bit5), PC=0, cycles=0
initCPU :: CPUState
initCPU = CPUState
    { cpuA      = 0
    , cpuX      = 0
    , cpuY      = 0
    , cpuSP     = 0xFD
    , cpuP      = 0x24
    , cpuPC     = 0
    , cpuMem    = emptyMem
    , cpuCycles = 0
    }

-- ---------------------------------------------------------------------------
-- Hand-rolled van Laarhoven lenses (type-compatible with microlens/lens)
-- ---------------------------------------------------------------------------

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

set :: Lens' s a -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)

over :: Lens' s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

-- ---------------------------------------------------------------------------
-- Register lenses
-- ---------------------------------------------------------------------------

regA :: Lens' CPUState Word8
regA f s = (\a -> s { cpuA = a }) <$> f (cpuA s)

regX :: Lens' CPUState Word8
regX f s = (\a -> s { cpuX = a }) <$> f (cpuX s)

regY :: Lens' CPUState Word8
regY f s = (\a -> s { cpuY = a }) <$> f (cpuY s)

regSP :: Lens' CPUState Word8
regSP f s = (\a -> s { cpuSP = a }) <$> f (cpuSP s)

regP :: Lens' CPUState Word8
regP f s = (\a -> s { cpuP = a }) <$> f (cpuP s)

regPC :: Lens' CPUState Word16
regPC f s = (\a -> s { cpuPC = a }) <$> f (cpuPC s)

mem :: Lens' CPUState Mem
mem f s = (\a -> s { cpuMem = a }) <$> f (cpuMem s)

cycles :: Lens' CPUState Int
cycles f s = (\a -> s { cpuCycles = a }) <$> f (cpuCycles s)

-- ---------------------------------------------------------------------------
-- Flag lenses (individual bits of the P register)
-- ---------------------------------------------------------------------------

flagBit :: Int -> Lens' CPUState Bool
flagBit n f s =
    let cur = testBit (cpuP s) n
    in  (\b -> s { cpuP = if b then cpuP s .|. mask else cpuP s .&. complement mask }) <$> f cur
  where
    mask = 1 `shiftL` n

flagC, flagZ, flagI, flagD, flagB, flagV, flagN :: Lens' CPUState Bool
flagC = flagBit 0
flagZ = flagBit 1
flagI = flagBit 2
flagD = flagBit 3
flagB = flagBit 4
flagV = flagBit 6
flagN = flagBit 7

-- ---------------------------------------------------------------------------
-- Composite lenses
-- ---------------------------------------------------------------------------

-- | Lens into a single byte of memory at a given address.
memAt :: Word16 -> Lens' CPUState Word8
memAt addr f s =
    let val = readByte addr (cpuMem s)
    in  (\v -> s { cpuMem = writeByte addr v (cpuMem s) }) <$> f val

-- | Update N and Z flags from a result byte.
updateNZ :: Word8 -> CPUState -> CPUState
updateNZ val = set flagN (testBit val 7) . set flagZ (val == 0)
