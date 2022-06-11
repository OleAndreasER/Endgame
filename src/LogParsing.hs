module LogParsing where

import TrainingLog

logsFromFileStr :: String -> [Log]
logsFromFileStr str = map parseLog $ lines str

parseLog :: String -> Log
parseLog str = Log {
    date = parseDate str,
    liftSessions = parseLiftSessions str
}

parseDate :: String -> String
parseDate = undefined

parseLiftSessions :: String -> [LiftSession]
parseLiftSessions = undefined
