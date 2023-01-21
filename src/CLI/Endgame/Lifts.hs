module CLI.Endgame.Lifts
    ( displayLifts
    , setPr
    , setCycle
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile )
import File.Profile
    ( readStats
    , toStats
    )
import Stats.Format
    ( format )
import Stats.Stats
    ( Stats
    , hasLift
    )
import qualified Stats.Stats as Stats
    ( setPr
    , setCycle
    )
import Types.General
    ( Lift
    , Weight
    )

displayLifts :: IO ()
displayLifts =
    ifProfile $
    putStrLn =<< format <$> readStats

toLift :: Lift -> (Stats -> Stats) -> IO ()
toLift lift f =
    ifProfile $
    hasLift lift <$> readStats >>= \hasLift' ->
    if hasLift'
    then toStats f >> displayLifts
    else putStrLn ("You don't do " ++ lift ++ ".")

setPr :: Lift -> Weight -> IO ()
setPr lift weight = toLift lift (Stats.setPr weight lift)

setCycle :: Lift -> Int -> Int -> IO ()
setCycle lift pos len = toLift lift (Stats.setCycle pos len lift)
