module FileHandling where

import System.Directory 
    ( getAppUserDataDirectory
    , listDirectory
    , doesFileExist
    , doesDirectoryExist
    )
import Data.Binary
import Types.Program (Program)
import Types.Log (Log)
import Types.Stats (Stats)
import Data.Functor ((<&>))


appPath :: IO FilePath
appPath = getAppUserDataDirectory "endgame"

readFromProfile :: Binary a => String -> IO a
readFromProfile file = do
    profile <- getProfile
    filePath <- appPath <&> (++"/profiles/"++profile++"/"++file)
    decodeFile filePath

setInProfile :: Binary a => String -> a -> IO ()
setInProfile file x = do
    profile <- getProfile  
    filePath <- appPath <&> (++"/profiles/"++profile++"/"++file)
    encodeFile filePath x

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
    filePath <- appPath <&> (++ "/programs/"++name++".txt") 
    encodeFile filePath program

readStandardProgram :: String -> IO Program
readStandardProgram programName =
    appPath <&> (++ "/programs/"++programName++".txt") >>= decodeFile


setProfile :: String -> IO ()
setProfile profile = do
    filePath <- appPath <&> (++ "/profile.txt")
    writeFile filePath profile

getProfiles :: IO [String]
getProfiles = do
    isProfiles' <- isProfiles
    if isProfiles'
    then appPath <&> (++ "/profiles/") >>= listDirectory
    else pure []

getProfile :: IO String
getProfile = appPath <&> (++ "/profile.txt") >>= readFile

profileIsSelected :: IO Bool
profileIsSelected = appPath <&> (++"/profile.txt") >>= doesFileExist

isProfiles :: IO Bool
isProfiles = appPath <&> (++ "/profiles/") >>= doesDirectoryExist
