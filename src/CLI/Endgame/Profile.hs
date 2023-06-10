module CLI.Endgame.Profile
    ( createProfile
    , switchToProfile
    , displayProfiles
    ) where

import File.ProfileManagement
    ( getProfiles
    , setCurrentProfile
    )
import qualified CLI.CreateProfile as CLI
    ( createProfile )
import Data.List (intercalate)

createProfile :: IO ()
createProfile = do
    putStrLn "Profile name:"
    profileName <- getLine
    isTaken <- elem profileName <$> getProfiles
    if isTaken
    then putStrLn $
        "There is already a profile named '" ++ profileName ++ "'."
    else CLI.createProfile profileName

switchToProfile :: String -> IO ()
switchToProfile profileName = do
    profileExists <- elem profileName <$> getProfiles
    if profileExists
    then do
        setCurrentProfile profileName
        putStrLn $ "Profile: " ++ profileName
    else
        putStrLn $ "There is no profile called '" ++ profileName ++ "'."

displayProfiles :: IO ()
displayProfiles = do
    profiles <- getProfiles
    if null profiles
        then putStrLn "There are no profiles."
        else do
            let profileList = (" - " ++) <$> profiles
            putStrLn "Profiles:"
            putStrLn $ unlines profileList
