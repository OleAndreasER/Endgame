module CLI.Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile
    )
import CLI.LogFormat
    ( formatLog
    )
import NextLogs
    ( getNextLogAndStats
    , getNextLogs
    )

displayNextLog :: IO ()
displayNextLog = ifProfile $ do
    (nextLog', _) <- getNextLogAndStats "Next:"
    putStrLn $ formatLog nextLog'

displayNextLogs :: Int -> IO ()
displayNextLogs n = ifProfile $ do
    logs <- take n <$> getNextLogs
    putStrLn $ unlines $ reverse $ map formatLog logs
