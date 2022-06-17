module NextLog where 

import qualified EndgameLog as Log
import qualified EndgameProgram as Program
import EndgameStats 

nextLog  :: [Log.Log]
         -> Stats
         -> Program.Program
         -> (Log.Log, Stats)

nextLog logs stats program =
    (Log.Log 
        { Log.date = ""
        , Log.liftSessions = undefined
        }
    , Stats undefined undefined
    )