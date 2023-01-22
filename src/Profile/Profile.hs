module Profile.Profile
    ( Profile
    , program
    , stats
    , logs
    , profile
    , newProfile
    , toStats
    , toStatsM
    , toProgram
    , toLogs
    ) where

import Program.Program
    ( Program )
import Stats.Stats
    ( Stats )
import qualified Stats.Stats as Stats
    ( fromProgram )
import Log.Log
    ( Log )

data Profile = Profile
    { program :: Program
    , stats :: Stats
    , logs :: [Log]
    } deriving (Show)

profile :: Program -> Stats -> [Log] -> Profile
profile = Profile

newProfile :: Program -> Profile
newProfile program = profile 
    program
    (Stats.fromProgram program)
    ([] :: [Log])

toStats :: (Stats -> Stats) -> Profile -> Profile
toStats f profile = profile { stats = f $ stats profile }

toStatsM :: Monad m => (Stats -> m Stats) -> Profile -> m Profile
toStatsM f profile = do
    stats' <- f $ stats profile
    pure $ profile { stats = stats' }

toProgram :: (Program -> Program) -> Profile -> Profile
toProgram f profile = profile { program = f $ program profile }

toLogs :: ([Log] -> [Log]) -> Profile -> Profile
toLogs f profile = profile { logs = f $ logs profile }
