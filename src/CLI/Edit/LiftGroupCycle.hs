module CLI.Edit.LiftGroupCycle (editLiftGroupCycle) where

import Types.Program as Program
import CLI.ProgramFormat (formatLiftGroupCycle)

editLiftGroupCycle :: LiftGroupCycle -> IO LiftGroupCycle
editLiftGroupCycle liftGroupCycle = do
    putStrLn "Editing:"
    putStrLn $ formatLiftGroupCycle liftGroupCycle
    putStrLn "Enter blank when you're happy with the new cycle."
    addLifts [] liftGroupCycle

addLifts :: LiftGroupCycle -> LiftGroupCycle -> IO LiftGroupCycle
addLifts new old = do
    putStrLn "\nEnter lift to add it to your new cycle:"
    putStrLn $ formatLiftGroupCycle new
    lift <- getLine
    handleLift lift
  where
    handleLift :: String -> IO LiftGroupCycle
    handleLift lift
        | lift `elem` old =
            addLifts (new ++ [lift]) old
        
        | lift == "" =
            return new

        | otherwise = do
            putStrLn $ lift ++ " is not a lift of this lift group cycle."
            addLifts new old
