module CLI.Endgame.Bodyweight 
    ( displayBodyweight
    , setBodyweight
    ) where

import Types.General
    ( Weight )
import File.Profile
    ( readStats
    , toStats
    )
import CLI.ArgumentEnsuring
    ( ifProfile )
import Stats.Stats
    ( bodyweight )
import qualified Stats.Stats as Stats
    ( setBodyweight )

displayBodyweight :: IO ()
displayBodyweight = ifProfile $ displayBodyweight'

displayBodyweight' :: IO ()
displayBodyweight' =
    readStats >>=
    putStrLn . ("Bodyweight: " ++ ) . (++ "kg") . show . bodyweight 

setBodyweight :: Weight -> IO ()
setBodyweight bw = ifProfile $ do
    toStats (Stats.setBodyweight bw)
    displayBodyweight'
