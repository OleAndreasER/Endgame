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
            putStrLn ("There are only "++ show logCount' ++ " logs.")
        Just log -> do
            toLogs (delete log)
            putStrLn "Removed log:"
            putStrLn $ format log
