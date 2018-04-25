module Lib where

import Text.Regex.PCRE.Heavy
import Data.ByteString (ByteString)
import Data.String.Interpolate

-- | Data structure for a single threshold configuration.
data Threshold = Threshold
  { thresholdName :: String
  , thresholdRegex :: ByteString
  , thresholdValue :: Double
  } deriving (Read, Show, Eq)

type Coverage = Double

-- | The result of evaluation. The _3 indicates whether the coverage passes the threshold or not.
type ThresholdEvaluationResult = (Threshold, Coverage, Bool)

-- | Extract the coverage from input string using a regex string
extractCoverage 
  :: String -- ^ The input string
  -> ByteString -- ^ The regex to extract the coverage. The regex should contain `(\\d+)` capture otherwise the coverage is always be 0.
  -> Coverage -- ^ The extracted coverage
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

-- | Evaluate the given string against the given threshold, producing an evaluation result
evaluate :: String -> Threshold -> ThresholdEvaluationResult
evaluate src threshold =
  let 
    coverage =
      extractCoverage src (thresholdRegex threshold)
    isPass =
      coverage >= thresholdValue threshold
  in
    (threshold, coverage, isPass)

-- | Produce a human-friendly output of the evaluation result
reportThreshold :: ThresholdEvaluationResult -> String
reportThreshold (Threshold tName _ tValue, coverage, isPass) =
  let
    remark = if isPass then "✓" else "·" :: String
    sign = if isPass then "≥" else "<" :: String
  in
    [i|#{remark} #{tName}: #{coverage}% (#{sign} #{tValue}%)|]

-- | Evaluate a string against a list of thresholds configuration and produce a report
evaluateAndReport
  :: String -- ^ The input string
  -> [Threshold] -- ^ The list of thresholds
  -> (String, Bool) -- ^ Pair of report and whether there is any coverage under thresholds
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

