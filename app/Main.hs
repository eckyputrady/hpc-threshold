module Main where

import HPCThreshold
import System.Exit

main :: IO ()
main = do
  thresholds <- read <$> readFile ".hpc-threshold"
  src <- getContents
  let (report, isPass) = evaluateAndReport src thresholds
  putStrLn report
  if isPass
    then exitWith ExitSuccess
    else exitWith $ ExitFailure 1
