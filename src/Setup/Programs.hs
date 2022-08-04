module Setup.Programs where

import Types.Program

programs :: [(String, Program)]
programs = 
    [ ("standard-half-days", standardHalfDays)
    , ("standard-all-days", standardAllDays)
    , ("pressfocus-half-days", pressfocusHalfDays)
    , ("norows-half-days", norowsHalfDays)
    ]


standardHalfDays :: Program
standardHalfDays = Program
    [["Press", "Bench"],
     ["Squat", "Deadlift", "Squat"],
     ["Chin", "Row"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Row",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]}]

pressfocusHalfDays :: Program
pressfocusHalfDays = Program 
    [["Press", "Bench", "Press"],
     ["Squat", "Deadlift", "Squat"],
     ["Chin", "Row"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Row",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]}]

standardAllDays :: Program
standardAllDays = Program 
    [["Press", "Bench"],
     ["Squat", "Deadlift", "Squat"],
     ["Chin", "Row"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Row",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]}]

norowsHalfDays :: Program
norowsHalfDays = Program
    [["Press", "Bench"],
     ["Squat", "Deadlift", "Squat"],
     ["Chin"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]}]
