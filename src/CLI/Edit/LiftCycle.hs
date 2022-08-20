module CLI.Edit.LiftCycle where

import Types.Program as Program
import Types.General as General
import CLI.ProgramFormat (formatLiftCycle, formatSetGroup)
import qualified CLI.Input.Defaulted as Defaulted
    ( getPositiveInteger
    , getString
    , getBool
    )
import qualified CLI.Input.Input as Input
    ( getPositiveInteger
    , getPercentage
    )

import Data.List (group)

editLiftCycle :: LiftCycle -> IO LiftCycle
editLiftCycle liftCycle = do
    putStrLn $ formatLiftCycle liftCycle
    putStrLn "We're gonna go through everything about this lift."
    putStrLn "Enter blank at any point if you want to keep it as it is."

    lift' <- getLiftName $ lift liftCycle
    prSession' <- getPrSession $ prSession liftCycle
    workSessionCycle' <- getWorkSessions $ workSessionCycle liftCycle
    pure $ LiftCycle
        { lift = lift'
        , prSession = prSession'
        , workSessionCycle = workSessionCycle'
        }

getLiftName :: Lift -> IO Lift
getLiftName default' =
    Defaulted.getString default' $ "Name of the lift ("++default'++"):"

getPrSession :: [Set] -> IO [Set]
getPrSession prSession = do 
    prSet <- getPrSet $ head prSession
    putStrLn "-Work sets after PR set-"
    workSets <- getSets $ tail prSession
    pure $ prSet : workSets

getPrSet :: Set -> IO Set
getPrSet prSet = do
    putStrLn "-PR set-"
    reps' <- Defaulted.getPositiveInteger
        (reps prSet) 
        $ "Reps on your PR set ("++show (reps prSet)++"): "
    pure $ prSet { reps = reps' }

getSets :: [Set] -> IO [Set]
getSets sets = do
    putStrLn $ "Your old sets: " 
    putStrLn $ formatSession sets
    shouldEdit <- Defaulted.getBool False "Do you want to edit these? (y/N)"
    if shouldEdit
    then putStrLn "First set group:" >> askForSets
    else pure sets

askForSets :: IO [Set]
askForSets = do
    sets <- Input.getPositiveInteger "Sets:"
    reps <- Input.getPositiveInteger "Reps:"
    percent <- Input.getPercentage "Percent of PR:"
    let set = Set reps percent Work
    let setGroup = take (fromIntegral sets) $ repeat set
    putStrLn $ formatSetGroup setGroup
    moreGroups <- Defaulted.getBool False "Do you want more set groups? (y/N)"
    if moreGroups
    then (setGroup ++) <$> askForSets
    else pure setGroup


getWorkSessions :: [[Set]] -> IO [[Set]]
getWorkSessions old = do
    putStrLn "You can have multiple work sessions in your cycle.\n\
             \Although one is recommended.\n\n\
             \These are your old work sessions:\n"
    putStrLn $ unlines $ formatSession <$> old
    shouldEdit <- Defaulted.getBool False "Do you want to edit these? (y/N)"
    if shouldEdit
    then askForSessions 1
    else pure old
  where
    askForSessions :: Int -> IO [[Set]]
    askForSessions i = do
        putStrLn $ "-Work Session "++ show i ++"-"
        sets <- askForSets
        moreSessions <- Defaulted.getBool False "Do you want more work sessions in your cycle? (y/N)"
        if moreSessions
        then (sets :) <$> askForSessions (i+1)
        else pure [sets]

formatSession :: [Set] -> String
formatSession sets =
    unlines
    $ map formatSetGroup
    $ group sets
