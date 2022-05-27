module Main where

import System.Environment
import Arguments (handleArguments)

main :: IO ()
main = do
    arguments <- getArgs
    handleArguments arguments