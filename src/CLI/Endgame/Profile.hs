module CLI.Endgame.Profile
    ( createProfile
    , switchToProfile
    ) where

import File.ProfileManagement
    ( getProfiles
    , setCurrentProfile
    )
import qualified CLI.CreateProfile as CLI
    ( createProfile )

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
