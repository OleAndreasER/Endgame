module Advance.PRs (advancePRs, regressPRs) where

import Types.Stats as Stats
import Types.Log as Log
import Types.General as General

advancePRs :: Log -> Stats -> Stats
advancePRs log stats = 
    foldr (setPRs 1) stats
    $ prLifts log

regressPRs :: Log -> Stats -> Stats
regressPRs log stats =
    foldr (setPRs (-1)) stats
    $ prLifts log

setPRs :: Int -> Lift -> Stats -> Stats
setPRs progressions = toLiftStats (addProgressions progressions)

prLifts :: Log -> [Lift]
prLifts log =
    map Log.lift
    $ filter hasPR
    $ liftSessions log
