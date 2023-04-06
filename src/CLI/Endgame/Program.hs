module CLI.Endgame.Program
    ( displayProfileProgram
    , setProgression
    , toggleBodyweight
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile
    , ensureIndex
    , ifLift
    )
import CLI.ProgramFormat
    ( formatProgram )
import File.Profile
    ( readProgram
    , toProgram
    )
import Types.General
    ( Lift
    , Weight
    )
import Program.Format
    ( format )
import Program.Program
    ( Program
    , hasLift
    )
import qualified Program.Program as Program
    ( setProgression
    , toggleBodyweight
    )

displayProfileProgram :: IO ()
displayProfileProgram = ifProfile displayProfileProgram'

displayProfileProgram' :: IO ()
displayProfileProgram' = putStrLn . format =<< readProgram

setProgression :: Lift -> Weight -> IO ()
setProgression lift weight =
    toLift lift (Program.setProgression lift weight)

toggleBodyweight :: Lift -> IO ()
toggleBodyweight lift =
    toLift lift (Program.toggleBodyweight lift)

toLift :: Lift -> (Program -> Program) -> IO ()
toLift lift f =
    ifProfile $
    hasLift lift <$> readProgram >>= \hasLift' ->
    if hasLift'
    then toProgram f >> displayProfileProgram'
    else putStrLn ("You don't do " ++ lift ++ ".")
