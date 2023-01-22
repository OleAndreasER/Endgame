module Profile.NextLog
    ( addLog
    , nextLog
    , nextLogs
    ) where

import Profile.Profile
    ( Profile )
import Log.Log
    ( Log )

addLog :: String -> Profile -> Profile
addLog label profile = undefined

nextLog :: Profile -> Log
nextLog profile = undefined

nextLogs :: Profile -> [Log]
nextLogs profile = undefined
