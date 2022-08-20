module CLI.Input.Defaulted where

import CLI.Input.Input (input)
import CLI.Input.Readers
    ( readPositiveInteger
    , readBool
    )
import Types.General as General

-- Input, except a default value is accepted when entering "".

readDefaulted :: (String -> Maybe a) -> a -> String -> Maybe a
readDefaulted _ default' ""   = Just default' 
readDefaulted readMaybe _ str = readMaybe str

inputDefaulted :: (String -> Maybe a) -> String -> a -> String -> IO a
inputDefaulted readMaybe error default' =
    input (readDefaulted readMaybe default') error

getPositiveInteger :: Integer -> String -> IO Integer
getPositiveInteger = inputDefaulted readPositiveInteger "Enter a positive integer."

getString :: String -> String -> IO String
getString = inputDefaulted Just ""

getBool :: Bool -> String -> IO Bool
getBool = inputDefaulted readBool "Enter 'y' or 'n'."
