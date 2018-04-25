module LibSpec where

import Test.Hspec
import qualified Control.Exception as E
import qualified Control.DeepSeq as D
import Lib

spec :: Spec
spec = do
  describe "Threshold" $
    it "should be parsable from string" $ do
      let threshold = Threshold "abc" "(\\d+)% abc" 80
      (read . show) threshold `shouldBe` threshold

  describe "extractCoverage" $ do
    it "should fail with invalid regexStr" $
      (E.evaluate . D.force) (extractCoverage "irrelevant" "ab(c")
        `shouldThrow` anyErrorCall
    it "should return 0 for regex with no digit capture" $
      extractCoverage "12% abc" "abc" `shouldBe` 0
    it "should return 0 if input string does not match regex" $
      extractCoverage "12% abc" "(\\d+) abc" `shouldBe` 0
    it "should capture the coverage" $
      extractCoverage "12% abc" "(\\d+)% abc" `shouldBe` 12

  describe "evaluate" $ do
    let threshold = Threshold "abc" "(\\d+)% abc" 80
    it "should correctly evaluate coverage < threshold as FAIL" $
      evaluate "10% abc" threshold `shouldBe` (threshold, 10, False)
    it "should correctly evaluate coverage == threshold as PASS" $
      evaluate "80% abc" threshold `shouldBe` (threshold, 80, True)
    it "should correctly evaluate coverage > threshold as PASS" $
      evaluate "100% abc" threshold `shouldBe` (threshold, 100, True)

  describe "reportThreshold" $ do
    it "should print passing result correctly" $ do
      let evalResult = (Threshold "abc" "(\\d+)% abc" 80, 80, True)
      reportThreshold evalResult `shouldBe` "✓ abc: 80.0% (≥ 80.0%)"
    it "should print failing result correctly" $ do
      let evalResult = (Threshold "abc" "(\\d+)% abc" 80, 70, False)
      reportThreshold evalResult `shouldBe` "· abc: 70.0% (< 80.0%)"

  describe "evaluateAndReport" $ do
    let thresholds =
          [ Threshold "abc" "(\\d+)% abc" 80
          , Threshold "def" "(\\d+)% def" 80
          ]
    it "should report passing result correctly" $
      evaluateAndReport "80% abc -- 80% def" thresholds `shouldBe`
        ( unlines [ "Code coverage threshold check: PASS"
                  , "✓ abc: 80.0% (≥ 80.0%)"
                  , "✓ def: 80.0% (≥ 80.0%)"
                  ]
        , True
        )
    it "should report failing result correctly" $
      evaluateAndReport "70% abc -- 80% def" thresholds `shouldBe`
        ( unlines [ "Code coverage threshold check: FAIL"
                  , "· abc: 70.0% (< 80.0%)"
                  , "✓ def: 80.0% (≥ 80.0%)"
                  ]
        , False
        )