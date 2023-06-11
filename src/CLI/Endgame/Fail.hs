module CLI.Endgame.Fail
    ( failLift
    ) where
import Types.General (Lift)
import File.Profile (readLog, readStats, setStats, toLog)
import Log.Log (failPr, wasPr, Log)
import Stats.Stats (Stats, toCycle)
import qualified Log.Format as Log (format)
import qualified Stats.Format as Stats (format)
import Relude (maybeToRight)
import Data.Maybe (fromJust)

noLogsErrorMessage :: String
noLogsErrorMessage = "There are no logs."
liftNotPresentMessage :: String -> String
liftNotPresentMessage lift = lift ++ " PR is not in your most recent log."

failLift :: Lift -> IO ()
failLift lift = do
    --Get most recent log
    firstLog' <- readLog 0
    let firstLog = maybeToRight noLogsErrorMessage firstLog'

    --Mark lift as failed in log
    let failPrEither lift log = maybeToRight (liftNotPresentMessage lift) $ failPr lift log
    let failedLog = firstLog >>= failPrEither lift

    case failedLog of
        Left error -> putStrLn error
        Right log -> do
            --Change cycle length of lift
            let isNowPr = wasPr log lift
            stats <- readStats
            let newStats = fromJust $ if isNowPr
                then decreaseCycleLength lift stats
                else increaseCycleLength lift stats

            --Store changes
            setStats newStats
            toLog 0 $ const log

            --Print changes
            putStrLn "\nLog:"
            putStrLn $ Log.format log

            putStrLn "\nLifts:"
            putStrLn $ Stats.format newStats

increaseCycleLength :: Lift -> Stats -> Maybe Stats
increaseCycleLength = toCycle increase
  where
    increase 0 1 = (1, 2)
    increase pos len = (pos, len + 1)

decreaseCycleLength :: Lift -> Stats -> Maybe Stats
decreaseCycleLength = toCycle decrease
  where
    decrease pos len = (pos `mod` (len - 1), len - 1)
