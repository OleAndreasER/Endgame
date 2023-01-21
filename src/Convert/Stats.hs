module Convert.Stats
    ( convert
    ) where

import qualified Types.Stats as Old
import qualified Stats.Stats as New
import Data.Maybe
    ( fromJust )

convert :: Old.Stats -> New.Stats -> New.Stats
convert old new =
    New.setBodyweight (Old.bodyweight old) $
    convertPositions old $
    foldr convertLiftStats new (Old.lifts old)

convertPositions :: Old.Stats -> New.Stats -> New.Stats
convertPositions old new =
    foldr convertPosition new
    (zip [0..] $ Old.liftGroupPositions old)

convertPosition :: (Int, Old.CyclePosition) -> New.Stats -> New.Stats
convertPosition (n, old) new =
    fromJust $ New.setLiftGroupPosition n (Old.position old) new

convertLiftStats :: Old.LiftStats -> New.Stats -> New.Stats
convertLiftStats old new =
    New.setCycle pos len lift $
    New.setPr (Old.pr old) lift $
    new
  where
    lift = Old.lift old
    Old.CyclePosition pos len = Old.liftCycle old
