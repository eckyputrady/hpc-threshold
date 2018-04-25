module Lib where

import Text.Regex.PCRE.Heavy
import Data.ByteString (ByteString)
import Data.String.Interpolate
import System.Exit

data Threshold = Threshold
  { thresholdName :: String
  , thresholdRegex :: ByteString
  , thresholdValue :: Double
  } deriving (Read, Show)

type Coverage = Double
type ThresholdEvaluationResult = (Threshold, Coverage, Bool)

extractCoverage :: String -> ByteString -> Coverage
extractCoverage src regexStr =
  case compileM regexStr [] of
    Left e ->
      error $ "Unable to compile regex " ++ show regexStr ++ ": " ++ e 
    Right regex ->
      case scan regex src of
        (_, coverage:_):_ ->
          read coverage
        _ ->
          0

evaluate :: String -> Threshold -> ThresholdEvaluationResult
evaluate src threshold =
  let 
    coverage =
      extractCoverage src (thresholdRegex threshold)
    isPass =
      coverage >= thresholdValue threshold
  in
    (threshold, coverage, isPass)

reportThreshold :: ThresholdEvaluationResult -> String
reportThreshold (Threshold tName _ tValue, coverage, isPass) =
  let
    remark = if isPass then "✓" else "·"
    sign = if isPass then "≥" else "<"
  in
    [i|#{remark} #{tName}: #{coverage}% (#{sign} #{tValue}%)|]

evaluateAndReport :: String -> [Threshold] -> (String, Bool)
evaluateAndReport src thresholds =
  let
    evalResults = map (evaluate src) thresholds
    isAllThresholdPass = all (\(_, _, isPass) -> isPass) evalResults
    reportSummary =
      if isAllThresholdPass
        then "Code coverage threshold check: PASS"
        else "Code coverage threshold check: FAIL"
    report = unlines $ reportSummary : map reportThreshold evalResults
  in
    (report, isAllThresholdPass)

evaluateAndReport' :: IO ()
evaluateAndReport' = do
  thresholds <- readFile ".hpc-threshold" >>= return . read
  src <- getContents
  let (report, isPass) = evaluateAndReport src thresholds
  putStrLn report
  if isPass
    then exitWith ExitSuccess
    else exitWith $ ExitFailure 1

