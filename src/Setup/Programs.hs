module Setup.Programs where

import Types.Program

programs :: [(String, Program)]
programs = 
    [ ("everyotherday", everyotherday)
    , ("everyday", everyday) ]


everyotherday :: Program
everyotherday = Program 
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

everyday :: Program
everyday = Program 
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