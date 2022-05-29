module ProgramParsing where

import EndgameProgram
import Data.List.Split (splitOn)

programFromFileStr :: String -> Program
programFromFileStr fileStr = Program (liftTypeCycles fileStr) (liftCycles fileStr)

liftTypeCycles :: String -> [LiftTypeCycle]
liftTypeCycles str =
    map LiftTypeCycle
    $ map (splitOn "-")
    $ map tail
    $ filter (startsWith '|')
    $ splitOn "\n" str

liftCycles :: String -> [LiftCycle]
liftCycles str =
    map liftCycle
    $ map (splitOn ":")
    $ map tail
    $ filter (startsWith '-')
    $ splitOn "\n" str

liftCycle :: [String] -> LiftCycle
liftCycle (liftStr:prStr:workStrs) =
    LiftCycle liftStr ([prSession] ++ workSessions)
        where prSession = LiftSession PR (sets prStr)
              workSessions = map (\workStr -> LiftSession Work (sets workStr)) workStrs

sets :: String -> [Set]
sets str = 
    foldr (++) []
    $ map setsBlock
    $ splitOn "&" str 

setsBlock :: String -> [Set]
setsBlock str =
    take setCount $ repeat $ Set reps percent sessionType
    where [setCountStr, repsStr, percentOrPRStr] = splitOn "x" str
          sessionType = if percentOrPRStr == "PR" then PR else Work
          percent = if percentOrPRStr == "PR" then 100.0 else read percentOrPRStr
          reps = read repsStr
          setCount = read setCountStr

startsWith :: Char -> String -> Bool
startsWith start str = head str == start