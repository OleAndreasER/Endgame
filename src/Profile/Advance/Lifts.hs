module Profile.Advance.Lifts
    ( advance
    ) where

import Stats.Stats
    ( toLiftGroupPositions )
import Program.Program
    ( Program
    , liftGroupCycles
    )
import Profile.Profile
    ( Profile
    , program
    , toStats
    )

advance :: Profile -> Profile
advance profile =
    toStats (
    toLiftGroupPositions (
    advanceLiftGroups (program profile))) profile

advanceLiftGroups :: Program -> [Int] -> [Int]
advanceLiftGroups program positions =
    advancePosition <$> cycles
  where
    lengths = length <$> liftGroupCycles program
    cycles :: [(Int, Int)]
    cycles = zip positions lengths

advancePosition :: (Int, Int) -> Int
advancePosition (pos, len) = (pos + 1) `mod` len
