module Main where

import Lib
import System.Exit

main :: IO ()
main = do
  thresholds <- readFile ".hpc-threshold" >>= return . read
  src <- getContents
  let (report, isPass) = evaluateAndReport src thresholds
  putStrLn report
  if isPass
    then exitWith ExitSuccess
    else exitWith $ ExitFailure 1
