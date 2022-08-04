module CLI.SelectProgram (selectProgram) where

import Types.Program as Program
import FileHandling (appPath, readStandardProgram)
import Data.Functor ((<&>))
import System.Directory (listDirectory)

selectProgram :: IO Program
selectProgram = do
    putStrLn "Which program do you want to run?"
    putStrLn =<< formatProgramSelect <$> getPrograms
    readStandardProgram =<< selectProgramInput =<< getPrograms 
     
selectProgramInput :: [String] -> IO String
selectProgramInput programs = do 
    answer <- getLine
    if answer `elem` programs
    then return answer
    else do
        putStrLn $ answer ++ " is not a program."
        selectProgramInput programs
    
getPrograms :: IO [String]
getPrograms = 
    appPath <&> (++ "/programs/")
    >>= listDirectory
    <&> map (takeWhile (/= '.'))

formatProgramSelect :: [String] -> String
formatProgramSelect names =
    init --remove newline
    $ unlines
    $ ("- "++) <$> names
