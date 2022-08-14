module CLI.Edit.LiftCycle where

import Types.Program as Program
import Types.General as General
import CLI.ProgramFormat (formatLiftCycle, formatSetGroup)
import CLI.Input.Defaulted (getPositiveInteger)
import Data.List (group)

editLiftCycle :: LiftCycle -> IO LiftCycle
editLiftCycle liftCycle = do
    putStrLn $ formatLiftCycle liftCycle
    putStrLn "We're gonna go through everything about this lift."
    putStrLn "Enter blank at any point if you don't want to change it."

    lift' <- getLiftName $ lift liftCycle
    prSession' <- getPrSession $ prSession liftCycle
    workSessionCycle' <- getWorkSessions $ workSessionCycle liftCycle
    pure $ LiftCycle
        { lift = lift'
        , prSession = prSession'
        , workSessionCycle = workSessionCycle'
        }

getLiftName :: Lift -> IO Lift
getLiftName oldName = do 
    putStrLn "Name of the lift: "
    orDefaultStr oldName <$> getLine

getPrSession :: [Set] -> IO [Set]
getPrSession prSession = do 
    putStrLn "-PR session-"
    prReps <- getPositiveInteger
        (reps $ head prSession) 
        "Reps of your PR set: "
    return prSession


getWorkSessions :: [[Set]] -> IO [[Set]]
getWorkSessions = undefined

orDefault :: Show a => a -> String -> String
orDefault default' "" = show default'
orDefault _ input     = input

orDefaultStr :: String -> String -> String
orDefaultStr default' "" = default'
orDefaultStr _ input     = input

--Useless?
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
