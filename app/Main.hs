module Main where

import Data.Set qualified as Set
import Numeric (showHex)

import ISA.Mos6502
import Target.C64

main :: IO ()
main = do
    let allOn   = c64TargetConfig defaultC64Subsystems
        noBasic = c64TargetConfig defaultC64Subsystems { useBasic = False }
        bare    = c64TargetConfig defaultC64Subsystems
                    { useBasic = False, useKernal = False }

    let sample = Instruction LDA (Immediate 0x42)
    putStrLn $ "Sample instruction: " ++ show sample
    putStrLn ""

    putStrLn "=== C64 Zero Page Allocation ==="
    putStrLn ""
    printConfig "All subsystems ON" allOn
    printConfig "BASIC OFF" noBasic
    printConfig "BASIC + KERNAL OFF" bare

printConfig :: String -> TargetConfig -> IO ()
printConfig label cfg = do
    let addrs = Set.toAscList (freeZeroPage cfg)
    putStrLn $ label ++ " (" ++ show (length addrs) ++ " bytes free):"
    putStrLn $ "  " ++ unwords (map fmt addrs)
    putStrLn ""
  where
    fmt w = '$' : showHex w ""
