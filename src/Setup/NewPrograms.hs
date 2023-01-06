module Setup.NewPrograms
    ( standardHalfDays
    , fiveLiftsHalfDays
    ) where

import Program.Program
    ( Program
    , program
    , lift
    , LiftInfo (..)
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
    
deadlift :: LiftInfo
deadlift = LiftInfo
    { name = "Deadlift"
    , progression = 2.5
    , isBodyweight = False
    }

chin :: LiftInfo
chin = LiftInfo
    { name = "Chin"
    , progression = 1.25
    , isBodyweight = True
    }

row :: LiftInfo
row = LiftInfo
    { name = "Row"
    , progression = 1.25
    , isBodyweight = False
    }


standardHalfDays :: Program
standardHalfDays = program
    [ ["Press", "Bench"]
    , ["Squat", "Deadlift"]
    , ["Chin", "Row"]
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
    , lift deadlift
        [ prSet "Deadlift" 3 : []
        , workSets "Deadlift" 2 5 87
        ]
    , lift chin
        [ prSet "Chin" 3 :
          workSets "Chin" 1 5 87
        , workSets "Chin" 3 5 87
        ]
    , lift row
        [ prSet "Row" 3 :
          workSets "Row" 1 5 87
        , workSets "Row" 3 5 87
        ]
    ]

fiveLiftsHalfDays :: Program
fiveLiftsHalfDays = program
    [ ["Press", "Bench"]
    , ["Squat", "Deadlift"]
    , ["Chin", "Row"]
    ]
    [ lift press
        [ prSet "Press" 3 :
          workSets "Bench" 2 7 81
        , workSets "Press" 2 5 87 <>
          workSets "Bench" 2 7 81
        ]
    , lift bench
        [ prSet "Bench" 3 :
          workSets "Press" 2 7 81
        , workSets "Bench" 2 5 87 <>
          workSets "Press" 2 7 81
        ]
    , lift squat
        [ prSet "Squat" 3 :
          workSets "Squat" 1 5 87
        , workSets "Squat" 3 5 87
        ]
    , lift deadlift
        [ prSet "Deadlift" 3 : []
        , workSets "Deadlift" 2 5 87
        ]
    , lift chin
        [ prSet "Chin" 3 :
          workSets "Row" 2 7 81
        , workSets "Chin" 2 5 87 <>
          workSets "Row" 2 7 81
        ]
    , lift row
        [ prSet "Row" 3 :
          workSets "Chin" 2 7 81
        , workSets "Row" 2 5 87 <>
          workSets "Chin" 2 7 81
        ]
    ]
