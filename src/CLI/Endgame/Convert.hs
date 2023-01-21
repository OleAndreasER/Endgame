module CLI.Endgame.Convert
    ( convertProfile
    ) where

import qualified Convert.Log as Log
    ( convert )
import qualified Convert.Stats as Stats
    ( convert )
import qualified Convert.Program as Program
    ( convert )
import qualified FileHandling as Old
    ( readProgram
    , readStats
    , readLogs
    )
import qualified File.Profile as New
    ( setProgram
    , setStats
    , setLogs
    )
import Stats.Stats
    ( fromProgram )

convertProfile :: IO ()
convertProfile = do
    newProgram <- Program.convert <$> Old.readStats <*> Old.readProgram

    oldStats <- Old.readStats
    let newStats = Stats.convert oldStats $ fromProgram newProgram

    oldLogs <- Old.readLogs
    let newLogs = Log.convert <$> oldLogs

    New.setProgram newProgram
    New.setStats newStats
    New.setLogs newLogs
