module Main where

import Test.Hspec
import LogFormatTest
    ( testLogFormat )
import StatsFormatTest
    ( testStatsFormat )

main :: IO ()
main = hspec $ do
    testLogFormat
    testStatsFormat
