module Main where

import Test.Hspec
import Log.Log
    ( Log
    , log
    , liftSession
    )
import Log.Set
    ( prSet
    , workSets
    , fail
    )
import Prelude hiding
    ( fail
    , log
    )
import qualified CLI.Format.Log as Format.Log
    ( format )

main :: IO ()
main = hspec testLogFormat

testLog :: Log
testLog = log "1/1/2023"
    [ liftSession "Bench" $
        fail (prSet "Bench" 3 100) :
        workSets "Press" 2 7 50
    , liftSession "Squat" $
        workSets "Squat" 3 5 135
    ]

expectedLogFormat =
    "1/1/2023\n\
    \Bench PR 1x3 100.0kg (FAIL)\n\
    \Press Work 2x7 50.0kg\n\
    \Squat Work 3x5 135.0kg"

testLogFormat :: Spec
testLogFormat = describe "CLI.Format" $ do
    it ".Log" $
        Format.Log.format testLog `shouldBe` expectedLogFormat
