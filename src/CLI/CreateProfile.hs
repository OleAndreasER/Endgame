module CLI.CreateProfile (createProfile) where

import Types.Log as Log
import System.Directory (createDirectoryIfMissing)
import CLI.SelectProgram (selectProgram)
import CLI.SetupStats (setupStats)
import FirstStatsOfProgram (firstStatsOfProgram)
import FileHandling 
    ( setProfile
    , setProgram
    , setLogs 
    , setStats
    , readStats
    , appPath
    )
import Data.Functor ((<&>))
   

{- A profile contains training logs, stats and program.
   There is a folder with the profile's name containing
   files for logs, stats and program.
-}

createProfile :: String -> IO ()
createProfile profile = do
    directory <- appPath <&> (++"/profiles/"++profile)

    createDirectoryIfMissing True directory
    setProfile profile

    program <- selectProgram 

    setProgram program
    setLogs ([] :: [Log])
    setStats $ firstStatsOfProgram program
    readStats >>= setupStats >>= setStats
