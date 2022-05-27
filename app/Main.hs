module Main where

import System.Environment
import Arguments (handleArguments)

main = do
    arguments <- getArgs
    handleArguments arguments