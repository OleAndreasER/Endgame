module CLI.Endgame.Remove
    ( removeLog
    ) where
import CLI.ArgumentEnsuring (ifProfile)
import File.Profile (readLog, logCount, toLogs)
import Log.Format (format)
import Data.List (delete)

-- n: positive integer
removeLog :: Int -> IO ()
removeLog n = ifProfile $ do
    maybeLog <- readLog (n - 1)
    case maybeLog of
        Nothing -> do
            logCount' <- logCount
            putStrLn $ notEnoughLogs logCount'
        Just log -> do
            toLogs (delete log)
            putStrLn "Removed log:"
            putStrLn $ format log

notEnoughLogs :: Int -> String
notEnoughLogs 0 = "There are no logs."
notEnoughLogs 1 = "There is only 1 log."
notEnoughLogs logCount = "There are only " ++ show logCount ++ " logs."
