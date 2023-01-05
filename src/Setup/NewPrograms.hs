module Setup.NewPrograms
    ( standardHalfDays
    , fiveLiftsHalfDays
    ) where

import Program.Program
    ( Program
    , program
    , lift
    )
import Program.Set
    ( prSet
    , workSets
    )

standardHalfDays :: Program
standardHalfDays = program
    [ ["Press", "Bench"]
    , ["Squat", "Deadlift"]
    , ["Chin", "Row"]
    ]
    [ lift "Press"
        [ prSet "Press" 3 :
          workSets "Press" 1 5 87
        , workSets "Press" 3 5 87
        ]
    , lift "Bench"
        [ prSet "Bench" 3 :
          workSets "Bench" 1 5 87
        , workSets "Bench" 3 5 87
        ]
    , lift "Squat"
        [ prSet "Squat" 3 :
          workSets "Squat" 1 5 87
        , workSets "Squat" 3 5 87
        ]
    , lift "Deadlift"
        [ prSet "Deadlift" 3 : []
        , workSets "Deadlift" 2 5 87
        ]
    , lift "Chin"
        [ prSet "Chin" 3 :
          workSets "Chin" 1 5 87
        , workSets "Chin" 3 5 87
        ]
    , lift "Row"
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
    [ lift "Press"
        [ prSet "Press" 3 :
          workSets "Bench" 2 7 81
        , workSets "Press" 2 5 87 <>
          workSets "Bench" 2 7 81
        ]
    , lift "Bench"
        [ prSet "Bench" 3 :
          workSets "Press" 2 7 81
        , workSets "Bench" 2 5 87 <>
          workSets "Press" 2 7 81
        ]
    , lift "Squat"
        [ prSet "Squat" 3 :
          workSets "Squat" 1 5 87
        , workSets "Squat" 3 5 87
        ]
    , lift "Deadlift"
        [ prSet "Deadlift" 3 : []
        , workSets "Deadlift" 2 5 87
        ]
    , lift "Chin"
        [ prSet "Chin" 3 :
          workSets "Row" 2 7 81
        , workSets "Chin" 2 5 87 <>
          workSets "Row" 2 7 81
        ]
    , lift "Row"
        [ prSet "Row" 3 :
          workSets "Chin" 2 7 81
        , workSets "Row" 2 5 87 <>
          workSets "Chin" 2 7 81
        ]
    ]
