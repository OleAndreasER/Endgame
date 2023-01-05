module Main where

import Test.Hspec

main :: IO ()
main = hspec simpleTest

simpleTest :: Spec
simpleTest = describe "H" $ do
    it "Should be right" $
        (4 + 5) `shouldBe` 9

