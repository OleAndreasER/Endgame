module FileHandling where

import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import Data.Binary
import Types.EndgameProgram (Program)
import Types.EndgameLog (Log, testLog)
import Types.EndgameStats (Stats, testStats)
import FirstStatsOfProgram


appPath :: IO FilePath
appPath = getAppUserDataDirectory "endgame"

readFromProfile :: Binary a => String -> IO a
readFromProfile file = do
    profile <- getProfile
    appPath' <- appPath
    decodeFile (appPath'++"/profiles/"++profile++"/"++file)

setInProfile :: Binary a => String -> a -> IO ()
setInProfile file x = do
    profile <- getProfile  
    appPath' <- appPath
    encodeFile (appPath'++"/profiles/"++profile++"/"++file) x

addLog :: Log -> IO ()
addLog log = do
    logs <- readLogs
    setInProfile "logs.txt" (log:logs)

setStats :: Stats -> IO ()
setStats = setInProfile "stats.txt"

setLogs :: [Log] -> IO ()
setLogs = setInProfile "logs.txt"

setProgram :: Program -> IO ()
setProgram = setInProfile "program.txt"


readStats :: IO Stats
readStats = readFromProfile "stats.txt"

readLogs :: IO [Log]
readLogs = readFromProfile "logs.txt"

readProgram :: IO Program
readProgram = readFromProfile "program.txt"


addProgram :: String -> Program -> IO ()
addProgram name program = do 
    appPath' <- appPath
    encodeFile (appPath'++"/programs/"++name++".txt") program

readStandardProgram :: String -> IO Program
readStandardProgram programName = do
    appPath' <- appPath
    decodeFile (appPath'++"/programs/"++programName++".txt")

--A profile contains training logs, stats and program. 
createProfile :: String -> String -> IO ()
createProfile profile programName = do
    appPath' <- appPath
    let directory = appPath'++"/profiles/"++profile

    createDirectoryIfMissing True directory

    program <- readStandardProgram programName --fix

    setProgram program
    setLogs ([] :: [Log])
    setStats $ firstStatsOfProgram program



    

setProfile :: String -> IO ()
setProfile profile = do 
    appPath' <- appPath
    writeFile (appPath'++"/profile.txt") profile
    
getProfile :: IO String
getProfile = do 
    appPath' <- appPath
    readFile (appPath'++"/profile.txt")