module CLI.Endgame.Bodyweight 
    ( displayBodyweight
    , setBodyweight
    ) where

import Types.General
    ( Weight
    )
import FileHandling
    ( readStats
    , setStats
    )
import CLI.ArgumentEnsuring
    ( ifProfile
    )
import Types.Stats
    ( bodyweight
    )

displayBodyweight :: IO ()
displayBodyweight =
    ifProfile $
    readStats >>= putStrLn . (++ "kg") . show . bodyweight 

setBodyweight :: Weight -> IO ()
setBodyweight bw = 
    ifProfile $ do
    readStats >>= setStats . \stats -> stats {bodyweight = bw}
    putStrLn ("Bodyweight: "++ show bw ++"kg")
