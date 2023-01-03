module Endgame.Profile
    ( createNewProfile
    , switchToProfile
    ) where

import FileHandling
    ( getProfiles
    , setProfile
    )
import CLI.CreateProfile
    ( createProfile
    )

createNewProfile :: IO ()
createNewProfile = do
    putStrLn "Profile name:"
    name <- getLine
    isProfile <- elem name <$> getProfiles
    if isProfile
    then putStrLn $ "There is already a profile named '"++name++"'."
    else createProfile name

switchToProfile :: String -> IO ()
switchToProfile profile = do
    isProfile <- elem profile <$> getProfiles
    if isProfile
    then do
        setProfile profile
        putStrLn $ "Profile: "++profile
    else
        putStrLn $ "There is no profile called '"++profile++"'."
