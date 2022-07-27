module Advance.PRs (advancePRs) where

import Types.Stats as Stats
import Types.Log as Log
import Types.General as General

advancePRs :: Log -> Stats -> Stats
advancePRs log stats = 
    foldr setPRs stats
    $ prLifts log

setPRs :: Lift -> Stats -> Stats
setPRs = toLiftStats (addProgressions 1)

prLifts :: Log -> [Lift]
prLifts log =
    map Log.lift
    $ filter hasPR
    $ liftSessions log
