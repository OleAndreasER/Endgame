module CLIProgramFormat where

import EndgameProgram

formatLiftGroupCycle :: LiftGroupCycle -> String
formatLiftGroupCycle (LiftGroupCycle lifts) =
    unwords
    (map ("-> " ++) lifts)
    ++ " ->" 

formatLiftGroupCycles :: [LiftGroupCycle] -> String
formatLiftGroupCycles cycles = 
    (++) "Lift group cycles:\n"
    $ unlines 
    $ map formatLiftGroupCycle cycles



{- 
Lift group cycles:
-> Press -> Bench ->
-> Squat -> Squat -> Deadlift ->
-> Chin -> Row ->

Lift cycles:
Press
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
         - 3x7x80%  [This is not in everyotherday]
Bench
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
Squat
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
Deadlift
    PR   - 1x3xPR
    Work - 2x5x87%
Chin
    PR   - 1x3xPR
           2x5x87% 
    Work - 4x5x87%
Row
    PR   - 1x3xPR
           2x5x87% 
    Work - 4x5x87%
-}