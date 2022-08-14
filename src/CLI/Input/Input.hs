module CLI.Input.Input where

import Types.General as General
import CLI.Input.Readers
    ( readWeight
    , readBool
    )

--Asks until it gets a valid input (Just a)

input :: (String -> Maybe a) -> String -> String -> IO a
input readMaybe error prompt = do
    putStrLn prompt
    answer <- getLine
    case readMaybe answer of
        Nothing -> do
            putStrLn error
            input readMaybe error prompt
        Just x  -> return x

getWeight :: String -> IO Weight
getWeight = input readWeight "Invalid weight"

getBool :: String -> IO Bool
getBool = input readBool "Enter 'y' or 'n'."
