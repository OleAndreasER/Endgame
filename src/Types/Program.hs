{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types.Program where
import GHC.Generics (Generic)
import Data.Binary
import Types.General (Reps, Percent, Lift)

instance Binary Program
instance Binary SetType
instance Binary Set
instance Binary LiftCycle

data Program = Program
    { liftGroupCycles :: [LiftGroupCycle] 
    , liftCycles :: [LiftCycle] 
    } deriving (Show, Read, Eq, Generic)


data SetType = PR | Work
    deriving (Show, Read, Eq, Generic)


data Set = Set 
    { reps :: Reps
    , percent :: Percent
    , setType :: SetType 
    } deriving (Show, Read, Eq, Generic)


type LiftGroupCycle = [Lift]
   

data LiftCycle = LiftCycle 
    { lift :: Lift
    , prSession :: [Set]
    , workSessionCycle :: [[Set]]
    } deriving (Show, Read, Eq, Generic)


cycleOfLift :: Lift -> Program -> LiftCycle
cycleOfLift lift' program = head
    $ filter ((lift' ==) . lift)
    $ liftCycles program

setLiftCycle :: Lift -> LiftCycle -> Program -> Program
setLiftCycle lift' newCycle program =
    program { liftCycles = maybeSet <$> liftCycles program }
  where
    maybeSet :: LiftCycle -> LiftCycle
    maybeSet oldCycle
        | lift oldCycle == lift' = newCycle
        | otherwise              = oldCycle

integrateLiftCycle :: Lift -> LiftCycle -> Program -> Program
integrateLiftCycle lift' newCycle program =
    renameInLiftGroupCycle lift' (lift newCycle)
    $ setLiftCycle lift' newCycle program

renameInLiftGroupCycle :: Lift -> Lift -> Program -> Program
renameInLiftGroupCycle old new program = program
    { liftGroupCycles =
        (map . map) (rename old new) $ liftGroupCycles program
    }

rename :: String -> String -> String -> String
rename old new str
    | str == old = new
    | otherwise  = str

sessions :: LiftCycle -> [[Set]]
sessions (LiftCycle { prSession, workSessionCycle }) =
    prSession : cycle workSessionCycle

setLiftGroupCycle :: Int -> LiftGroupCycle -> Program -> Program
setLiftGroupCycle n lgc program =
    let (xs, _:ys) = splitAt n $ liftGroupCycles program
    in program { liftGroupCycles = xs ++ (lgc:ys) }

liftInProgram :: Program -> Lift -> Bool
liftInProgram program lift' =
    any ((lift' ==) . lift)
    $ liftCycles program
