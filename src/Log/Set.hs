{-# LANGUAGE DeriveGeneric #-}

module Log.Set
    ( Set
        (..)
    , SetType
        (..)
    , set
    , bodyweightSet
    , prSet
    , workSets
    , fail
    ) where

import GHC.Generics
    ( Generic )
import Data.Binary
import Types.General
    ( Reps
    , Weight
    , Lift
    )
import Prelude hiding
    ( fail )
import qualified Program.Set as Program
    ( Set
    , lift
    , reps
    , percent
    , setType
    , SetType (..)
    )
import Data.Aeson (ToJSON, FromJSON)

data SetType = Work | PR Bool
    deriving (Generic, Show, Read, Eq)

instance Binary SetType
instance ToJSON SetType
instance FromJSON SetType

data Set = Set
    { lift :: Lift
    , reps :: Reps
    , weight :: Weight
    , setType :: SetType
    } deriving (Generic, Show, Read, Eq)

instance Binary Set
instance ToJSON Set
instance FromJSON Set

set :: Weight -> Weight -> Program.Set -> Set
set = bodyweightSet 0

bodyweightSet :: Weight -> Weight -> Weight -> Program.Set -> Set
bodyweightSet bodyweight progression pr programSet = Set
    { lift = Program.lift programSet 
    , reps = Program.reps programSet
    , weight =
        roundTo progression $
        pr * Program.percent programSet / 100 - bodyweight
    , setType = case Program.setType programSet of
        Program.PR -> PR True
        Program.Work -> Work
    }

prSet :: Lift -> Reps -> Weight -> Set
prSet lift reps weight = Set lift reps weight (PR True)

workSets :: Lift -> Int -> Reps -> Weight -> [Set]
workSets lift sets reps weight =
    replicate sets (Set lift reps weight Work)

fail :: Set -> Maybe Set
fail set = case setType set of
    Work         -> Nothing
    PR succeeded -> Just set
        { setType = PR (not succeeded) }


roundTo :: Float -> Float -> Float
roundTo multiple n = multiple * multiples
  where
    multiples =
        fromIntegral $ round (n / multiple) :: Float
