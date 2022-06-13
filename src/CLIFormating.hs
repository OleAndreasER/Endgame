module CLIFormating where
import EndgameLog

formatLog :: Log -> String
formatLog log = (date log) ++ (formatLiftSessions $ liftSessions log)

formatLiftSessions :: [LiftSession] -> String
formatLiftSessions liftSessions = 
    unlines
    $ map ("    " ++)
    $ map formatLiftSession
    $ liftSessions

formatLiftSession :: LiftSession -> String
formatLiftSession liftSession = 
    unlines
    $ map ("    " ++)
    $ map (lift liftSession ++)
    $ map formatSet
    $ sets liftSession

formatSet :: Set -> String
formatSet set = PRStr ++ " " ++ set 



--"06/05/2022:
--    Bench PR 1x3 100kg
--    Bench Work 1x5 87kg 
--    Squat PR 1x3 200kg (FAIL)
--    Squat Work 1x5 170
--    Row Work 3x5 75