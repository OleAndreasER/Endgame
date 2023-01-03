module Endgame.Program 
    ( displayProgram
    , displayLiftGroupCycle
    , editLiftGroupCycle
    , displayProgramLift
    , editProgramLift
    ) where

import CLI.ArgumentEnsuring
    ( ifProfile
    , ensureIndex
    , ifLift
    )
import CLI.ProgramFormat
    ( formatProgram
    )
import FileHandling
    ( readProgram
    , setProgram
    , readStats
    , setStats
    )
import Types.General
    ( Lift
    )
import qualified Types.Program as Program
    ( lift
    )
import Types.Stats
    ( CyclePosition (CyclePosition)
    , setLiftGroupPosition
    , renameLift
    )
import Types.Program
    ( integrateLiftCycle
    , cycleOfLift
    , setLiftGroupCycle
    , liftGroupCycles
    )
import CLI.Edit.LiftCycle
    ( editLiftCycle
    )
import qualified CLI.Edit.LiftGroupCycle as Edit.LGC
    ( editLiftGroupCycle
    )
import CLI.ProgramFormat
    ( formatLiftCycle
    , formatLiftGroupCycle
    )

displayProgram :: IO ()
displayProgram = 
    ifProfile $
    readProgram >>= putStrLn . formatProgram

displayLiftGroupCycle :: Int -> IO ()
displayLiftGroupCycle n =
    ifProfile $ do
    cycles <- liftGroupCycles <$> readProgram
    ensureIndex n cycles $ putStrLn . formatLiftGroupCycle

editLiftGroupCycle :: Int -> IO ()
editLiftGroupCycle n =
    ifProfile $
    liftGroupCycles <$> readProgram >>= \cycles ->
    ensureIndex n cycles $ \oldCycle -> do
    newCycle <- Edit.LGC.editLiftGroupCycle oldCycle
    readProgram >>= setProgram . setLiftGroupCycle (n-1) newCycle
    let resetCycle = CyclePosition 0 $ length newCycle
    readStats >>= setStats . setLiftGroupPosition (n-1) resetCycle

displayProgramLift :: Lift -> IO ()
displayProgramLift lift =
    ifProfile $
    ifLift lift $
    putStrLn =<< formatLiftCycle . cycleOfLift lift <$> readProgram

editProgramLift :: Lift -> IO ()
editProgramLift lift =
    ifProfile $
    ifLift lift $ do
    edited <- editLiftCycle =<< cycleOfLift lift <$> readProgram
    readProgram >>= setProgram . integrateLiftCycle lift edited
    readStats >>= setStats . renameLift lift (Program.lift edited)
