module CLI.ArgumentEnsuring where

import Types.General
import Types.Log
import FileHandling
import Text.Read (readMaybe)

--Converting and then ensuring valid arguments.

ensure :: Either String a -> (a -> IO ()) -> IO ()
ensure x f = either putStrLn f x

readFloat :: String -> Either String Float
readFloat str = case readMaybe str :: Maybe Float of
    Just n  -> Right n
    Nothing -> Left $ "'"++str++"' is not a number."

readInt :: String -> Either String Int
readInt str = case readMaybe str :: Maybe Int of
    Just n  -> Right n
    Nothing -> Left $ "'"++str++"' is not an integer."

check :: Show a => (a -> Bool) -> String -> a -> Either String a
check predicate aboutX x = if predicate x
    then Right x
    else Left $ "'"++ show x ++"' "++ aboutX 

getLog :: [Log] -> Int -> Either String Log
getLog logs n = if n > length logs
    then Left ("There are only "++(show $ length logs)++" logs.")
    else Right $ logs !! (n-1)

ensureWeight :: String -> (Weight -> IO ()) -> IO ()
ensureWeight str = ensure 
    $ readFloat str
    >>= check (>= 0) "can't be negative." 

ensureNonNegativeInt :: String -> (Int -> IO ()) -> IO ()
ensureNonNegativeInt str = ensure
    $ readInt str
    >>= check (>= 0) "can't be negative."

ensurePositiveInt :: String -> (Int -> IO ()) -> IO ()
ensurePositiveInt str = ensure
    $ readInt str
    >>= check (> 0) "must be positive."

ensureCycle :: String -> String -> (Int -> Int -> IO ()) -> IO ()
ensureCycle posStr lenStr f =
    ensurePositiveInt posStr $ \pos ->
    ensurePositiveInt lenStr $ \len ->
    ensure (check (<= len) outOfBounds pos)
    $ \pos' -> f pos' len
  where 
    outOfBounds =
        "is larger than '"++lenStr++"'. Meaning it's out of the cycle's bounds."

ensureLog :: String -> (Log -> IO ()) -> IO ()
ensureLog nStr f = do
    logs <- readLogs
    ensure (readInt nStr >>= check (> 0) "must be positive." >>= getLog logs) f