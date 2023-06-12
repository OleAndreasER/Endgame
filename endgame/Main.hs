module Main where

import System.Environment ( getArgs )
import CLI.Arguments (handleArguments)

main :: IO ()
main = do 
    getArgs >>= handleArguments
