{-# LANGUAGE NamedFieldPuns #-}
module Advance.PRs (advancePRs) where

import Types.Stats as Stats
import Types.Log as Log
import Types.General as General

advancePRs :: Log -> Stats -> Stats
advancePRs log stats =
    foldr ($) stats
    $ map (uncurry $ setPRs)
    $ prLifts log

prLifts :: Log -> [(Lift, Weight)]
prLifts log = 
    map liftPR
    $ filter hasPR
    $ liftSessions log

liftPR :: LiftSession -> (Lift, Weight)
liftPR (LiftSession { Log.lift, sets }) =
    (lift, weight $ head sets)

setPRs :: Lift -> Weight -> Stats -> Stats
setPRs lift pr stats = 
    toLiftStats (setPR pr') lift stats
    where
        pr' = accountForBodyweight lift pr stats


