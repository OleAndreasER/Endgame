module Arguments where

import System.Environment
import Data.List

listArguments = do
    args <- getArgs
    putStrLn (answer args)

answer :: [String] -> String
answer ["next"] = "Squats and stuff (big weight)"
answer _ = "fail"
