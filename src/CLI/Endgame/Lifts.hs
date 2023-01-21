module CLI.Endgame.Lifts
    ( displayLifts
    , setPR
    , setProgression
    , setCycle
    , toggleBodyweight
    ) where

import Types.General
    ( Weight
    , Lift
    )
import CLI.StatsFormat
    ( formatStats
    )
import FileHandling
    ( readStats
    , setStats
    )
import Types.Stats
    ( LiftStats
    , addProgressions
    , liftIsInStats
    , toLiftStats
    )
import qualified Types.Stats as Stats
    ( setPR
    , setProgression
    , setCycle
    , toggleBodyweight
    )
import CLI.ArgumentEnsuring
    ( ifProfile )

displayLifts :: IO ()
displayLifts = 
    ifProfile $
    readStats >>= putStrLn . formatStats

setPR :: Lift -> Weight -> IO ()
setPR lift weight =
    ifProfile $
    updateLifts lift $ Stats.setPR weight

setProgression :: Lift -> Weight -> IO ()
setProgression lift weight =  
    ifProfile $
    updateLifts lift $ Stats.setProgression weight
    
setCycle :: Lift -> Int -> Int -> IO ()
setCycle lift pos len =
    ifProfile $
    updateLifts lift $ Stats.setCycle pos len

toggleBodyweight :: Lift -> IO ()
toggleBodyweight lift =
    ifProfile $
    updateLifts lift Stats.toggleBodyweight


updateLifts :: Lift -> (LiftStats -> LiftStats) -> IO ()
updateLifts lift f = do
    stats <- readStats
    if liftIsInStats lift stats
    then do
        let newStats = toLiftStats f lift stats
        setStats newStats
        putStrLn $ formatStats newStats
    else
        putStrLn $ "You don't do "++lift++"."