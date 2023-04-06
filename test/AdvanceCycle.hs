module AdvanceCycle
    ( testAdvanceCycle
    ) where
import Test.Hspec ( it, shouldBe, Spec )
import Stats.LiftStats
    ( LiftStats (cyclePosition)
    , newLiftStats
    , setCycle
    , cyclePosition, advanceCycle
    )

testAdvanceCycle :: Spec
testAdvanceCycle =
    it "Advance Cycle" $
    cyclePosition (advanceCycle liftStats) `shouldBe` 0

liftStats :: LiftStats
liftStats =
    setCycle 1 2
    newLiftStats
