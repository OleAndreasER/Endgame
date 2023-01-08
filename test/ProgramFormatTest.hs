module ProgramFormatTest
    ( testProgramFormat
    ) where

import Test.Hspec
import TestProgram
    ( testProgram )
import Program.Format
    ( format )

testProgramFormat :: Spec
testProgramFormat =
    it "Program Format" $
    format testProgram `shouldBe` expectedProgramFormat

expectedProgramFormat :: String
expectedProgramFormat =
    "Lift group cycles:\n\
    \-> Press -> Bench -> Press ->\n\
    \-> Squat ->\n\
    \-> Chin ->\n\n\
    \Lift cycles:\n\
    \Press (+1.25kg)\n\
    \    - Press 1x3 PR\n\
    \      Press 1x5 87.0%\n\
    \    - Press 3x5 87.0%\n\
    \Bench (+1.25kg)\n\
    \    - Bench 1x3 PR\n\
    \      Bench 1x5 87.0%\n\
    \    - Bench 3x5 87.0%\n\
    \Squat (+2.5kg)\n\
    \    - Squat 1x3 PR\n\
    \      Squat 1x5 87.0%\n\
    \    - Squat 3x5 87.0%\n\
    \Chin (+1.25kg) (Bodyweight)\n\
    \    - Chin 1x3 PR\n\
    \      Chin 1x5 87.0%\n\
    \    - Chin 3x5 87.0%"
