{-# LANGUAGE NamedFieldPuns #-}
module Advance.PRs (advancePRs) where

import Types.Stats as Stats
import Types.Log as Log
import Types.General as General

prLifts :: Log -> [Lift]
prLifts log =
    map Log.lift
    $ filter hasPR
    $ liftSessions log

setPRs :: Lift -> Stats -> Stats
setPRs = toLiftStats (addProgressions 1)

advancePRs :: Log -> Stats -> Stats
advancePRs log stats = 
    foldr setPRs stats
    $ prLifts log
