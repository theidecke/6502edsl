module Main where

import System.Exit (exitFailure)

import Test.ISA qualified
import Test.Instructions qualified
import Test.Monad qualified
import Test.Control qualified
import Test.Memory qualified
import Test.Target qualified

main :: IO ()
main = do
    ok <- fmap and . sequence $ concat
        [ Test.ISA.tests
        , Test.Instructions.tests
        , Test.Monad.tests
        , Test.Control.tests
        , Test.Memory.tests
        , Test.Target.tests
        ]
    if ok then putStrLn "\nAll properties passed."
          else putStrLn "\nSome properties FAILED." >> exitFailure
