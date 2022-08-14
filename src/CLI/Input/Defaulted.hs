module CLI.Input.Defaulted where

import CLI.Input.Input (input)
import CLI.Input.Readers (readPositiveInteger)

-- Input, except a default value is accepted when entering "".

readDefaulted :: (String -> Maybe a) -> a -> String -> Maybe a
readDefaulted _ default' ""   = Just default' 
readDefaulted readMaybe _ str = readMaybe str

inputDefaulted :: (String -> Maybe a) -> String -> a -> String -> IO a
inputDefaulted readMaybe error default' =
    input (readDefaulted readMaybe default') error

getPositiveInteger :: Integer -> String -> IO Integer
getPositiveInteger = inputDefaulted readPositiveInteger "Enter a positive integer."
