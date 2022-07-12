module Main where

import System.Environment
import CLI.Arguments (handleArguments)
import Setup.Setup

main :: IO ()
main = do 
    setup
    getArgs >>= handleArguments