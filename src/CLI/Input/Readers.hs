module CLI.Input.Readers where

import Control.Monad ((<=<))
import Text.Read (readMaybe)
import Types.General as General

readWeight :: String -> Maybe Weight 
readWeight = maybeIf (>= 0) <=< readMaybe 

readBool :: String -> Maybe Bool
readBool "y" = Just True
readBool "n" = Just False
readBool _   = Nothing

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf p x = if p x then Just x else Nothing

readPositiveInteger :: String -> Maybe Integer
readPositiveInteger = maybeIf (> 0) <=< readMaybe 

readPercentage :: String -> Maybe Percent
readPercentage =
    maybeIf (> 0)
    <=< readMaybe
    . popPercentSymbol

popPercentSymbol :: String -> String
popPercentSymbol str
    | last str == '%' = init str
    | otherwise       = str
