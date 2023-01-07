module Main where

import Test.Hspec
import LogFormatTest
    ( testLogFormat )

main :: IO ()
main = hspec testLogFormat
