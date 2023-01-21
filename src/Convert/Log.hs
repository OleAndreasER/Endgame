module Convert.Log
    ( convert
    ) where

import qualified Types.Log as Old
import qualified Log.Log as New
import qualified Log.Session as New.Session
import qualified Log.Set as New.Set
import Types.General
    ( Lift )

convert :: Old.Log -> New.Log
convert old = New.log
    (Old.label old)
    (convertLiftSession <$> Old.liftSessions old)

convertLiftSession :: Old.LiftSession -> (Lift, New.Session.Session)
convertLiftSession old = New.liftSession
    (Old.lift old)
    ((convertSet $ Old.lift old) <$> Old.sets old)

convertSet :: Lift -> Old.Set -> New.Set.Set
convertSet lift (Old.Set reps weight (Old.PR wasPr)) = 
    applyIf New.Set.fail (not wasPr) $
    New.Set.prSet lift reps weight

convertSet lift (Old.Set reps weight Old.Work) =
    head $ New.Set.workSets lift 1 reps weight

applyIf :: (a -> a) -> Bool -> a -> a
applyIf f proposition x = if proposition then f x else x
