module CLI.SetupStats where

import Types.Stats

{- Goes through each lift,
   and asks pr, etc. 
-}

setupStats :: Stats -> IO Stats
setupStats stats = do
    setupBodyweight' <- setupBodyweight
    setupLifts <- sequence $ map setupLift $ lifts stats
    return $ stats
        { lifts = setupLifts
        , bodyweight = setupBodyweight' }

setupLift :: LiftStats -> IO LiftStats
setupLift = return

setupBodyweight :: IO Bodyweight
setupBodyweight = return 5