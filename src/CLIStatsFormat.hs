module CLIStatsFormat (formatStats) where

import EndgameStats

formatStats :: Stats -> String
formatStats stats =
    "Bodyweight: "++(show $ bodyweight stats)++"\n"
    ++(unlines $ map formatLift $ lifts stats)

formatLift :: LiftStats -> String
formatLift (LiftStats lift _ pr liftCycle) =
    lift++": "++(show pr)++"kg "++(formatLiftCyclePosition liftCycle)


formatLiftCyclePosition :: CyclePosition -> String
formatLiftCyclePosition (CyclePosition pos len) = 
    (show (pos+1))++"/"++(show len)