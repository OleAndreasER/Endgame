module CLI.CreateProfile
    ( createProfile
    ) where

import System.Directory
    ( createDirectoryIfMissing )
import Stats.Stats
    ( Stats )
import qualified Stats.Stats as Stats
    ( fromProgram
    , setPr
    , setBodyweight
    , liftList
    )
import Types.General
    ( Lift )
import Program.Program
    ( Program )
import File.ProfileManagement
    ( setCurrentProfile )
import File.Profile
    ( setProfile )
import Profile.Profile
    ( toStatsM
    , newProfile
    )
import qualified File.Path as Path
    ( profile )
import Data.Foldable
    ( foldrM )
import CLI.Input.Input
    ( getWeight )
import Setup.Programs
    ( programs )
import Data.Maybe
    ( fromJust )

{- A profile contains training logs, stats and program.
   There is a folder with the profile's name containing
   logs.txt, stats.txt and program.txt.
-}

createProfile :: String -> IO ()
createProfile profileName = do
    profile <- toStatsM setupStats . newProfile =<< selectProgram
    createDirectoryIfMissing True =<< Path.profile profileName
    setCurrentProfile profileName
    setProfile profile

selectProgram :: IO Program
selectProgram = do
    putStrLn "Which program do you want to run?"
    putStrLn $ formatProgramNames (fst <$> programs)
    selectedProgramName <- getSelectedProgram (fst <$> programs)
    pure $ fromJust $ lookup selectedProgramName programs

getSelectedProgram :: [String] -> IO String
getSelectedProgram programs = do
    answer <- getLine
    if answer `elem` programs
    then pure answer
    else do
        putStrLn $ answer ++ " is not a program."
        getSelectedProgram programs

formatProgramNames :: [String] -> String
formatProgramNames names =
    init $ unlines $ ("- " ++) <$> names

setupStats :: Stats -> IO Stats
setupStats stats = do
    stats' <- setBodyweight stats
    putStrLn "Enter starting PR for each lift: "
    setPrs stats'

setPrs :: Stats -> IO Stats
setPrs stats = foldrM setPr stats $ Stats.liftList stats

setPr :: Lift -> Stats -> IO Stats
setPr lift stats = do
    pr <- getWeight $ "- " ++ lift
    pure $ Stats.setPr pr lift stats

setBodyweight :: Stats -> IO Stats
setBodyweight stats = do
    bodyweight <- getWeight bodyweightPrompt
    pure $ Stats.setBodyweight bodyweight stats

bodyweightPrompt :: String
bodyweightPrompt =
    "Your bodyweight will get subtracted from weighted bodyweight exercises.\n\
    \Remember to update it frequently if you do any of those.\n\
    \Bodyweight: "
