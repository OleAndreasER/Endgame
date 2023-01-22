module CLI.Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile )
import Profile.NextLog
    ( nextLog
    , nextLogs
    )
import File.Profile
    ( readProfile )
import Log.Format
    ( format )

displayNextLog :: IO ()
displayNextLog =
    ifProfile $
    putStrLn =<< format . nextLog <$> readProfile

displayNextLogs :: Int -> IO ()
displayNextLogs n = ifProfile $ do
    logs <- take n . nextLogs <$> readProfile
    putStrLn $ unlines $ reverse $ format <$> logs
