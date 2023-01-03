module Endgame.Log
    ( displayLogs
    , displayLog
    , failLiftInLog
    , removeLog
    , updateLifts -- temp location
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile
    , ensureLog
    )
import CLI.LogFormat
    ( formatLog
    )
import CLI.StatsFormat
    ( formatStats
    )
import FileHandling
    ( readLogs
    , setLogs
    , readStats
    , setStats
    , readProgram
    )
import qualified Types.Log as Log
    ( failLift
    )
import Types.Log
    ( SetType (Work, PR)
    , liftSetType
    )
import Types.General
    ( Lift
    )
import qualified Types.Stats as Stats
    ( addWork
    )
import Types.Stats
    ( LiftStats
    , addProgressions
    , liftIsInStats
    , toLiftStats
    )
import Advance.Cycles
    ( regressCycles
    )
import Advance.PRs
    ( regressPRs
    )

displayLogs :: Int -> IO ()
displayLogs logs =
    ifProfile $
    readLogs >>=
    putStrLn . unlines . map formatLog . reverse . take logs

displayLog :: Int -> IO ()
displayLog n =
    ifProfile $ 
    ensureLog n $
    putStrLn . formatLog

failLiftInLog :: Lift -> Int -> IO ()
failLiftInLog lift logIndex = 
    ifProfile $
    ensureLog logIndex $ \log -> do
    readLogs >>= setLogs . (toElem log $ Log.failLift lift)

    let newLog = Log.failLift lift log
    putStrLn $ formatLog newLog

    let setType = liftSetType lift newLog
    case setType of
        Nothing -> return ()
        Just Work -> putStrLn "You can't fail a work set."
        Just (PR True)  -> unfailLift lift
        Just (PR False) -> failLift lift

removeLog :: Int -> IO ()
removeLog 1 = 
    ifProfile $
    ensureLog 1 $ \log -> do

    readLogs >>= setLogs . tail
    readStats >>= setStats . regressPRs log
    setStats =<< regressCycles <$> readProgram <*> readStats

    putStrLn "Removed:"
    putStrLn $ formatLog log
    putStrLn "After undoing PRs and cycle advances, this is your stats:"
    readStats >>= putStrLn . formatStats

removeLog n =
    ifProfile $
    ensureLog n $ \log -> do
    setLogs =<< removeAt (n - 1) <$> readLogs

    putStrLn "Removed:"
    putStrLn $ formatLog log

failLift :: Lift -> IO ()
failLift lift = do
    putStrLn $ "Subtracted 2 progression's worth of weight from "++lift++"'s PR."
    addWork 1 lift
    updateLifts lift (addProgressions (-2))

unfailLift :: Lift -> IO ()
unfailLift lift = do
    putStrLn $ "Added back 2 progression's worth of weight to "++lift++"'s PR."
    addWork (-1) lift
    updateLifts lift (addProgressions 2)

addWork :: Int -> Lift -> IO ()
addWork work lift = do
    putStrLn $ workTxt ++ "\n"
    readStats >>= setStats . Stats.addWork work lift
  where 
    workTxt 
        | work == 0  = ""
        | work == 1  = "Added a work day to "++lift++"."
        | work == -1 = "Removed a work day from "++lift++"."
        | work > 1   = "Added "++show work++" work days to "++lift++"."
        | work < -1  = "Removed "++show work++" work days from "++lift++"."

updateLifts :: Lift -> (LiftStats -> LiftStats) -> IO ()
updateLifts lift f = do
    stats <- readStats
    if liftIsInStats lift stats
    then do
        let newStats = toLiftStats f lift stats
        setStats newStats
        putStrLn $ formatStats newStats
    else
        putStrLn $ "You don't do "++lift++"."

--Applies f to the first instance of y in a list
toElem :: Eq a => a -> (a -> a) -> [a] -> [a]
toElem y f (x:xs)
    | y == x    = f x : xs
    | otherwise = x : toElem y f xs

removeAt :: Int -> [a] -> [a]
removeAt n xs 
    | n < length xs = left ++ right
    | otherwise     = xs
    where (left, (_ :right)) = splitAt n xs
