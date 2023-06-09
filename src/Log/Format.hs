module Log.Format
    ( format
    ) where

import Data.List
    ( group )
import Log.Log
    ( Log
    , label
    , sessions
    )
import Log.Session
    ( Session )
import Log.Set
    ( Set
        (..)
    , SetType
        (..)
    )

format :: Log -> String
format log =
    label log ++ "\n" ++
    init (concatMap formatSession (sessions log))

formatSession :: Session -> String
formatSession session =
    concatMap formatSetGroup (group session)

formatSetGroup :: [Set] -> String
formatSetGroup sets =
    lift set ++ " " ++
    formatSetType (setType set) ++ " " ++
    show setCount ++ "x" ++
    show (reps set) ++ " " ++
    show (weight set) ++ "kg" ++
    maybeFail (setType set) ++ "\n"
  where
    set = head sets
    setCount = length sets
    maybeFail (PR False) = " (FAIL)"
    maybeFail _          = ""

formatSetType :: SetType -> String
formatSetType (PR _) = "PR"
formatSetType Work   = "Work"
