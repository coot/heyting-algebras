{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Algebra.Lattice ( JoinSemiLattice
                       , BoundedJoinSemiLattice
                       , MeetSemiLattice
                       , BoundedMeetSemiLattice
                       , Lattice
                       , BoundedLattice
                       )
import Algebra.Lattice.Lifted (Lifted (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Algebra.Boolean
import Algebra.Heyting

newtype HArbitrary t a = HArbitrary {runHArbitrary :: t a }
  deriving ( JoinSemiLattice
           , BoundedJoinSemiLattice
           , MeetSemiLattice
           , BoundedMeetSemiLattice
           , Lattice
           , BoundedLattice
           , BooleanAlgebra
           , HeytingAlgebra
           , Eq
           , Ord
           , Show
           )

instance Arbitrary a => Arbitrary (HArbitrary Lifted a) where
  arbitrary = HArbitrary . maybe Bottom Lift <$> arbitrary
  shrink (HArbitrary Bottom)   = []
  shrink (HArbitrary (Lift a)) =
    HArbitrary Bottom : [ HArbitrary (Lift a') | a' <- shrink a ]

newtype Composed f g a = Composed (f (g a))
  deriving ( JoinSemiLattice
           , BoundedJoinSemiLattice
           , MeetSemiLattice
           , BoundedMeetSemiLattice
           , Lattice
           , BoundedLattice
           , BooleanAlgebra
           , HeytingAlgebra
           , Eq
           , Ord
           , Show
           )

instance Arbitrary a => Arbitrary (Composed Lifted Lifted a) where
  arbitrary = frequency
    [ (1, return $ Composed Bottom)
    , (1, return $ Composed (Lift Bottom))
    , (2, Composed . Lift . Lift  <$> arbitrary)
    ]

  shrink (Composed Bottom)          = []
  shrink (Composed (Lift Bottom))   = [Composed Bottom]
  shrink (Composed (Lift (Lift a))) =
       [ Composed Bottom, Composed (Lift Bottom) ]
    ++ [ Composed (Lift (Lift a')) | a' <- shrink a ]

main :: IO ()
main =
  hspec $ do
    prop "BooleanAlgebra Bool" (prop_BooleanAlgebra @Bool)
    prop "BooleanAlgebra (Bool, Bool)" (prop_BooleanAlgebra @(Bool, Bool))
    prop "BooleanAlgebra HBool (Lifted Bool)" (prop_BooleanAlgebra @(HBool (HArbitrary Lifted Bool)))

    prop "HeytingAlgebra Bool" (prop_HeytingAlgebra @(Bool))
    prop "HeytingAlgebra (Lifted Bool)" (prop_HeytingAlgebra @(HArbitrary Lifted Bool))
    prop "HeytingAlgebra (Lifted (Lifted Bool))" (prop_HeytingAlgebra @(Composed Lifted Lifted Bool))
