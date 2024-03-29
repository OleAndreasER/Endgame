module CLI.ArgumentEnsuring where

import Types.General ( Weight )
import Text.Read (readMaybe)
import File.ProfileManagement ( profileIsSelected )
import File.Profile ( readProgram )

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

ensureIndex :: Int -> [a] -> (a -> IO ()) -> IO ()
ensureIndex n xs f
    | n > length xs =
        putStrLn $ "'"++ show n ++ "' is too large."

    | n <= 0 =
        putStrLn $ "'"++ show n ++ "' must be positive."

    | otherwise =
        f $ xs !! (n-1)

ifProfile :: IO () -> IO ()
ifProfile f = do
    profileIsSelected' <- profileIsSelected
    if profileIsSelected'
    then f
    else putStrLn "You must create a profile first."
