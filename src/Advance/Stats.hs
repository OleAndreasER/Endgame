module Advance.Stats (advanceStats) where 

import Advance.PRs
import Advance.Cycles
import Types.Stats
import Types.Log

advanceStats :: Log -> Stats -> Stats
advanceStats = advanceCycles <.> advancePRs

(<.>) :: (a -> c -> d) -> (a -> b -> c) -> (a -> b -> d)
f <.> g = \x -> f x . g x
