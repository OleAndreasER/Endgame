module File.ProfileManagement
    ( setProfile
    , getProfiles
    , getProfile
    , profileIsSelected
    , profilesFolderExists
    ) where

import qualified File.Path as Path
    ( currentProfile )
import System.Directory 
    ( listDirectory
    , doesFileExist
    , doesDirectoryExist
    )

setProfile :: String -> IO ()
setProfile profileName = do
    profilePath <- Path.currentProfile
    writeFile profilePath profileName

getProfiles :: IO [String]
getProfiles = do
    folderExists <- profilesFolderExists
    if folderExists 
    then listDirectory =<< Path.currentProfile
    else pure []

getProfile :: IO String
getProfile = readFile =<< Path.currentProfile

profileIsSelected :: IO Bool
profileIsSelected = doesFileExist =<< Path.currentProfile

profilesFolderExists :: IO Bool
profilesFolderExists = doesDirectoryExist =<< Path.currentProfile
