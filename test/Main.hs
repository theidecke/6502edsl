module Main where

import System.Exit (exitFailure)

import Test.ISA qualified
import Test.Instructions qualified
import Test.Monad qualified
import Test.Control qualified
import Test.Memory qualified
import Test.Target qualified
import Test.Emu.Mem qualified
import Test.Emu.CPU qualified
import Test.Emu.Step qualified
import Test.Emu.Trace qualified
import Test.Emu.Integration qualified
import Test.Emu.Dormann qualified
import Test.Emu.Laziness qualified

main :: IO ()
main = do
    ok <- fmap and . sequence $ concat
        [ Test.ISA.tests
        , Test.Instructions.tests
        , Test.Monad.tests
        , Test.Control.tests
        , Test.Memory.tests
        , Test.Target.tests
        , Test.Emu.Mem.tests
        , Test.Emu.CPU.tests
        , Test.Emu.Step.tests
        , Test.Emu.Trace.tests
        , Test.Emu.Integration.tests
        , Test.Emu.Dormann.tests
        , Test.Emu.Laziness.tests
        ]
    if ok then putStrLn "\nAll properties passed."
          else putStrLn "\nSome properties FAILED." >> exitFailure
