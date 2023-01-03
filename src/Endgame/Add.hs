module Endgame.Add
    ( addNextLog
    ) where

import NextLogs
    ( getNextLogAndStats
    )
import FileHandling
    ( addLog
    , setStats
    )
import CLI.LogFormat
    ( formatLog
    )
import CLI.ArgumentEnsuring
    ( ifProfile
    )
import Date
    ( dateStr
    )

addNextLog :: IO ()
addNextLog = ifProfile $ do
    (nextLog', nextStats') <- getNextLogAndStats =<< dateStr
    addLog nextLog'
    putStrLn $ "Added:\n" ++ formatLog nextLog'
    setStats nextStats'
