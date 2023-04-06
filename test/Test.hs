module Main where

import Test.Hspec
import LogFormatTest
    ( testLogFormat )
import StatsFormatTest
    ( testStatsFormat )
import ProgramFormatTest
    ( testProgramFormat )
import AdvanceCycle
    ( testAdvanceCycle )

main :: IO ()
main = hspec $ do
    testLogFormat
    testStatsFormat
    testProgramFormat
    testAdvanceCycle
