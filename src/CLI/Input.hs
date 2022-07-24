module CLI.Input where

import Text.Read (readMaybe)
import Types.General
import Control.Monad ((<=<))

getWeight :: String -> IO Weight
getWeight = input readWeight "Invalid weight"

getBool :: String -> IO Bool
getBool = input readBool "Enter 'y' or 'n'."

input :: (String -> Maybe a) -> String -> String -> IO a
input readMaybe' error prompt = do
    putStrLn prompt
    answer <- getLine
    case readMaybe' answer of
        Nothing -> do
            putStrLn error
            input readMaybe' error prompt
        Just x  -> return x

readWeight :: String -> Maybe Weight 
readWeight = maybeIf (>= 0) <=< readMaybe 

readBool :: String -> Maybe Bool
readBool "y" = Just True
readBool "n" = Just False
readBool _   = Nothing

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf p x = if p x then Just x else Nothing