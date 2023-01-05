module Main where

import Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Nothing" $ do
    it "n/a" $
        (2 - 1) `shouldBe` 1