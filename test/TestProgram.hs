module TestProgram 
    ( testProgram
    ) where

import Program.Program
    ( Program
    , program
    , lift
    )
import Program.LiftInfo
    ( LiftInfo 
        (..)
    )
import Program.Set
    ( prSet
    , workSets
    )

press :: LiftInfo
press = LiftInfo
    { name = "Press"
    , progression = 1.25
    , isBodyweight = False
    }

bench :: LiftInfo
bench = LiftInfo
    { name = "Bench"
    , progression = 1.25
    , isBodyweight = False
    }

squat :: LiftInfo
squat = LiftInfo
    { name = "Squat"
    , progression = 2.5
    , isBodyweight = False
    }

chin :: LiftInfo
chin = LiftInfo
    { name = "Chin"
    , progression = 1.25
    , isBodyweight = True
    }

testProgram :: Program
testProgram = program
    [ ["Press", "Bench", "Press"]
    , ["Squat"]
    , ["Chin"]
    ]
    [ lift press
        [ prSet "Press" 3 :
          workSets "Press" 1 5 87
        , workSets "Press" 3 5 87
        ]
    , lift bench
        [ prSet "Bench" 3 :
          workSets "Bench" 1 5 87
        , workSets "Bench" 3 5 87
        ]
    , lift squat
        [ prSet "Squat" 3 :
          workSets "Squat" 1 5 87
        , workSets "Squat" 3 5 87
        ]
    , lift chin
        [ prSet "Chin" 3 :
          workSets "Chin" 1 5 87
        , workSets "Chin" 3 5 87
        ]
    ]
