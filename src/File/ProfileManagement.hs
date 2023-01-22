module File.ProfileManagement
    ( setCurrentProfile
    , getProfiles
    , getProfile
    , profileIsSelected
    , profilesFolderExists
    ) where

import qualified File.Path as Path
    ( currentProfile
    , profilesFolder
    )
import System.Directory 
    ( listDirectory
    , doesFileExist
    , doesDirectoryExist
    )

setCurrentProfile :: String -> IO ()
setCurrentProfile profileName = do
    profilePath <- Path.currentProfile
    writeFile profilePath profileName

getProfiles :: IO [String]
getProfiles = do
    folderExists <- profilesFolderExists
    if folderExists 
    then listDirectory =<< Path.profilesFolder
    else pure []

getProfile :: IO String
getProfile = readFile =<< Path.currentProfile

profileIsSelected :: IO Bool
profileIsSelected = doesFileExist =<< Path.currentProfile

profilesFolderExists :: IO Bool
profilesFolderExists = doesDirectoryExist =<< Path.profilesFolder
