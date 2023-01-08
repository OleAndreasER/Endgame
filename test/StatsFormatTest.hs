module StatsFormatTest
    ( testStatsFormat
    ) where

import Test.Hspec
import TestProgram
    ( testProgram )
import Stats.Stats
    ( Stats
    , fromProgram
    , setBodyweight
    , setPr
    , setCycle
    )
import Stats.Format
    ( format )

testStatsFormat :: Spec
testStatsFormat =
    it "Stats Format" $
    format testStats `shouldBe` expectedStatsFormat

testStats :: Stats
testStats = 
    setBodyweight 72.5 $
    setPr 60 "Press" $
    setPr 100 "Bench" $
    setPr 160 "Squat" $
    setPr 80 "Chin" $
    setCycle 1 3 "Press" $
    setCycle 2 4 "Bench" $
    setCycle 2 2 "Squat" $ 
    setCycle 1 2 "Chin" $
    fromProgram testProgram

expectedStatsFormat :: String
expectedStatsFormat =
    "Bodyweight: 72.5kg\n\
    \Press: 60.0kg 1/3\n\
    \Bench: 100.0kg 2/4\n\
    \Squat: 160.0kg 2/2\n\
    \Chin: 80.0kg 1/2"
