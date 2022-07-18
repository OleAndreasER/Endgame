module CLI.SetupStats where

import Types.Stats
import CLI.Input

{- Goes through each lift,
   and asks pr, etc. 
-}

setupStats :: Stats -> IO Stats
setupStats stats = do
    bw <- getWeight "Your bodyweight will get subtracted from weighted bodyweight exercises.\n\
                     \Remember to update it frequently if you do any of those.\n\
                     \Bodyweight: "
    setupLifts <- sequence $ map setupLift $ lifts stats
    return $ stats
        { lifts = setupLifts
        , bodyweight = bw }

setupLift :: LiftStats -> IO LiftStats
setupLift liftStats = do
    putStrLn $ "-"++ lift liftStats ++ ": " 
    pr' <- getWeight "Starting PR weight: "
    progression' <- getWeight "Progression increment: "
    isBodyweight' <- getBool "Should your bodyweight be subtracted from the weight?\n\
                             \This is mainly for weighted chins, rows etc. (y/n)"
    return $ liftStats 
        { pr = pr' 
        , progression = progression'
        , isBodyweight = isBodyweight' }
