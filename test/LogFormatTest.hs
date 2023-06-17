module LogFormatTest
    ( testLogFormat
    ) where

import Test.Hspec
import Prelude hiding
    ( log
    , fail
    )
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
import Log.Format
    ( format )
import Data.Maybe (fromJust)

testLogFormat :: Spec
testLogFormat =
    it "Log Format" $
    format testLog `shouldBe` expectedLogFormat

testLog :: Log
testLog = log "1/1/2023"
    [ liftSession "Bench" $
        (fromJust $ fail (prSet "Bench" 3 100)) :
        workSets "Press" 2 7 50
    , liftSession "Squat" $
        workSets "Squat" 3 5 135
    ]

expectedLogFormat :: String
expectedLogFormat =
    "1/1/2023\n\
    \Bench PR 1x3 100.0kg (FAIL)\n\
    \Press Work 2x7 50.0kg\n\
    \Squat Work 3x5 135.0kg"
