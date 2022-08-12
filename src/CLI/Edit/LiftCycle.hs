module CLI.Edit.LiftCycle where

import Types.Program as Program
import Types.General as General
import CLI.ProgramFormat (formatLiftCycle, formatSetGroup)
import Data.List (group)

editLiftCycle :: LiftCycle -> IO LiftCycle
editLiftCycle liftCycle = do
    putStrLn $ formatLiftCycle liftCycle

    lift' <- getLiftName $ lift liftCycle
    prSession' <- getPrSession
    workSessionCycle' <- getWorkSessions
    pure $ LiftCycle
        { lift = lift'
        , prSession = prSession'
        , workSessionCycle = workSessionCycle'
        }

getLiftName :: Lift -> IO Lift
getLiftName oldName = undefined

getPrSession :: IO [Set]
getPrSession = undefined

getWorkSessions :: IO [[Set]]
getWorkSessions = undefined





selectionPrompt :: LiftCycle -> String
selectionPrompt liftCycle = 
    (++) (lift liftCycle ++ "\n")
    $ unlines
    $ map (uncurry formatSets)
    $ zip [1 ..]
    $ prSession liftCycle : workSessionCycle liftCycle

formatSets :: Int -> [Set] -> String
formatSets n sets =
    (++) (show n ++ ": ")
    $ unlines
    $ map formatSetGroup
    $ group sets
