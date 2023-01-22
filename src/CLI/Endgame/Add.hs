module CLI.Endgame.Add
    ( addNextLog
    ) where

import Profile.NextLog
    ( addLog )
import File.Profile
    ( readLog
    , toProfile
    )
import Log.Format
    ( format )
import CLI.ArgumentEnsuring
    ( ifProfile )
import Date
    ( dateStr )
import Data.Maybe
    ( fromJust )

addNextLog :: IO ()
addNextLog = ifProfile $ do
    dateStr' <- dateStr
    toProfile (addLog dateStr')
    putStrLn =<< format <$> (fromJust <$> readLog 1)
