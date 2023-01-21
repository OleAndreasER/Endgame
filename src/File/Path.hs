module File.Path
    ( stats
    , program
    , logs
    , currentProfile
    ) where

import System.Directory 
    ( getAppUserDataDirectory )
import Data.Functor
    ((<&>))

endgame :: IO FilePath
endgame = getAppUserDataDirectory "endgame"

profile :: String -> IO FilePath
profile profileName = endgame <&> (++ "/profiles/" ++ profileName)

stats :: String -> IO FilePath
stats profileName = profile profileName <&> (++ "/stats.txt")

program :: String -> IO FilePath
program profileName = profile profileName <&> (++ "/program.txt")

logs :: String -> IO FilePath
logs profileName = profile profileName <&> (++ "/logs.txt")

currentProfile :: IO FilePath
currentProfile = endgame <&> (++ "/profile.txt")
