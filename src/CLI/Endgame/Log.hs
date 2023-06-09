module CLI.Endgame.Log
    ( displayLogs
    , displayLog
    --, failLiftInLog
    --, removeLog
    --, updateLifts -- temp location
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile )
import Types.General
    ( Lift )
import Log.Format
    ( format )
import File.Profile
    ( readLogs
    , readLog
    , logCount
    )

-- logCount: positive integer
displayLogs :: Int -> IO ()
displayLogs logCount = ifProfile $ do
    logs <- readLogs logCount
    putStrLn $ unlines $ reverse $ (++"\n") <$> format <$> logs

-- n: positive integer
displayLog :: Int -> IO ()
displayLog n = ifProfile $ do
    maybeLog <- readLog (n - 1)
    case maybeLog of
        Just log -> putStrLn $ format log
        Nothing  -> do
            logCount' <- logCount
            putStrLn ("There are only "++ show logCount' ++ " logs.")

{-
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
-}
