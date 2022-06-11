module LogParsing where

import TrainingLog
import Data.List.Split (splitOn)

logsFromFileStr :: String -> [Log]
logsFromFileStr str = map parseLog $ lines str

parseLog :: String -> Log
parseLog str = Log {
    date = dateStr,
    liftSessions = parseLiftSessions $ liftSessionsStr
}   where (dateStr:liftSessionsStr) = splitOn ";" str

parseLiftSessions :: [String] -> [LiftSession]
parseLiftSessions = map parseLiftSession

parseLiftSession :: String -> LiftSession
parseLiftSession str = LiftSession {
    lift = liftStr,
    sessionType = parseSessionType sessionTypeStr,
    sets = parseSets setsStr 
}   where [liftStr, sessionTypeStr, setsStr] = splitOn "," str 

parseSessionType :: String -> SetType
parseSessionType = read

parseSets :: String -> [Set]
parseSets str = undefined 