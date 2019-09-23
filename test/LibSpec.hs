module LibSpec where

import Lib (addOne, subOne)
import Test.Hspec
import Text.Printf (printf)

testAddOne :: Int -> Int -> Spec
testAddOne input output =
  it (printf "should addOne : %d --> %d" input output) $
  addOne input `shouldBe` output

testSubOne :: Int -> Int -> Spec
testSubOne input output =
  it (printf "should subOne : %d --> %d" input output) $
  subOne input `shouldBe` output

spec :: Spec
spec = do
  describe "addOne" $ do
    testAddOne 2 3
    testAddOne 3 4
  describe "subOne" $ do
    testSubOne 4 3
    testSubOne 3 2
