module Main where

import System.Environment
import CLI.Arguments (handleArguments)

main :: IO ()
main = do
    arguments <- getArgs
    handleArguments arguments