module BowlingSpec
  ( spec
  ) where

import Bowling
  ( Frame(Simple, Spare, Strike)
  , FrameNumber
  , addFrames
  , frameValue
  , getOneRoll
  , parse
  , rollValue
  , scoreGame
  )
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScoreGame :: String -> Int -> Spec
testScoreGame game score =
  it (printf "should return the score for game : %s --> %d" game score) $ scoreGame game `shouldBe` score

testParse :: String -> [Frame] -> Spec
testParse game frames = it (printf "should return frames : %s" game) $ parse game `shouldBe` frames

testAddFrames :: [(FrameNumber, Frame)] -> Int -> Spec
testAddFrames [] val =
  it (printf "should return value : %s --> %d" "[]" val) $ addFrames [] `shouldBe` val
testAddFrames (frame:frames) val =
  it (printf "should return value : %s --> %d" (show [frame:frames]) val) $ addFrames (frame:frames) `shouldBe` val

testFrameValue :: FrameNumber -> Frame -> Int -> Spec
testFrameValue fnum frame val =
  it (printf "should return value : %d %s --> %d" fnum (show frame) val) $ frameValue (fnum, frame) `shouldBe` val

testGetOneRoll :: FrameNumber -> Frame -> Int -> Spec
testGetOneRoll fnum frame val =
  it (printf "should return value : %d %s --> %d" fnum (show frame) val) $ getOneRoll (fnum, frame) `shouldBe` val

testRollValue :: Char -> Int -> Spec
testRollValue c i = it (printf "should return value : %d --> %d" c i) $ rollValue c `shouldBe` i

spec :: Spec
spec = do
  describe "scoreGame" $ do
    testScoreGame "--------------------" 0
    testScoreGame "1-1----------------1" 3
    testScoreGame "9-9-9-9-9-9-9-9-9-9-" 90
    testScoreGame "1-1-1-1-1-1-1-1-1-1-" 10
    testScoreGame "12131415161718171611" 59
    testScoreGame "5/5/5/5/5/5/5/5/5/5/5" 150
    testScoreGame "XXXXXXXXXXXX" 300
    testScoreGame "XXXXXXXXXX12" 274
    testScoreGame "1/35XXX458/X3/23" 160
    testScoreGame "1/35XXX458/X3/XX6" 189
    testScoreGame "5/11------------3/11" 26
  describe "parse" $ do
    testParse "" []
    testParse "1" [Simple '1' '-']
    testParse "X" [Strike]
    testParse "X1" [Strike, Simple '1' '-']
    testParse "XXX" [Strike, Strike, Strike]
    testParse "X1X" [Strike, Simple '1' '-', Strike]
    testParse "X21" [Strike, Simple '2' '-', Simple '1' '-']
    testParse "XXXX" [Strike, Strike, Strike, Strike]
    testParse "XX21" [Strike, Strike, Simple '2' '-', Simple '1' '-']
    testParse "4/XX" [Spare '4', Strike, Strike]
    testParse "4/21" [Spare '4', Simple '2' '1']
    testParse "9-9-9-9-9-9-9-9-9-9-" (replicate 10 (Simple '9' '-'))
    testParse "5/5/5/5/5/5/5/5/5/5/5" (replicate 10 (Spare '5') ++ [Simple '5' '-'])
    testParse
      "1/35XXX458/X3/23"
      [Spare '1', Simple '3' '5', Strike, Strike, Strike, Simple '4' '5', Spare '8', Strike, Spare '3', Simple '2' '3']
  describe "rollValue" $ do
    testRollValue '-' 0
    testRollValue '1' 1
  describe "frameValue" $ do
    testFrameValue 1 Strike 10
    testFrameValue 10 Strike 10
    testFrameValue 1 (Spare '4') 4
    testFrameValue 10 (Spare '4') 4
    testFrameValue 1 (Simple '2' '6') 8
    testFrameValue 10 (Simple '2' '6') 8
  describe "getOneRoll" $ do
    testGetOneRoll 1 Strike 10
    testGetOneRoll 10 Strike 10
    testGetOneRoll 3 (Spare '5') 5
    testGetOneRoll 1 (Simple '2' '3') 2
    testGetOneRoll 1 (Simple '2' '4') 2
  describe "addFrames" $ do
    testAddFrames [] 0
    testAddFrames [(1, Strike)] 10
