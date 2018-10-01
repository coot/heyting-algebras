{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Algebra.Lattice ( JoinSemiLattice
                       , BoundedJoinSemiLattice
                       , MeetSemiLattice
                       , BoundedMeetSemiLattice
                       , Lattice
                       , BoundedLattice
                       )
import Algebra.Lattice.Dropped (Dropped (..))
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

instance Arbitrary a => Arbitrary (HArbitrary Dropped a) where
  arbitrary = HArbitrary . maybe Top Drop <$> arbitrary
  shrink (HArbitrary Top)   = []
  shrink (HArbitrary (Drop a)) =
    HArbitrary Top : [ HArbitrary (Drop a') | a' <- shrink a ]

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

instance Arbitrary a => Arbitrary (Composed Dropped Lifted a) where
  arbitrary = frequency
    [ (1, return $ Composed Top)
    , (1, return $ Composed (Drop Bottom))
    , (2, Composed . Drop . Lift  <$> arbitrary)
    ]

  shrink (Composed Top)             = []
  shrink (Composed (Drop Bottom))   = [Composed Top]
  shrink (Composed (Drop (Lift a))) =
       [ Composed Top, Composed (Drop Bottom) ]
    ++ [ Composed (Drop (Lift a')) | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Composed Lifted Dropped a) where
  arbitrary = frequency
    [ (1, return $ Composed Bottom)
    , (1, return $ Composed (Lift Top))
    , (2, Composed . Lift . Drop  <$> arbitrary)
    ]

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

instance Arbitrary a => Arbitrary (Composed Dropped Dropped a) where
  arbitrary = frequency
    [ (1, return $ Composed Top)
    , (1, return $ Composed (Drop Top))
    , (2, Composed . Drop . Drop  <$> arbitrary)
    ]

  shrink (Composed Top)          = []
  shrink (Composed (Drop Top))   = [Composed Top]
  shrink (Composed (Drop (Drop a))) =
       [ Composed Top, Composed (Drop Top) ]
    ++ [ Composed (Drop (Drop a')) | a' <- shrink a ]

main :: IO ()
main =
  hspec $ do
    prop "BooleanAlgebra Bool" (prop_BooleanAlgebra @Bool)
    prop "BooleanAlgebra (Bool, Bool)" (prop_BooleanAlgebra @(Bool, Bool))
    prop "BooleanAlgebra HBool (Lifted Bool)" (prop_BooleanAlgebra @(HBool (HArbitrary Lifted Bool)))

    prop "HeytingAlgebra Bool" (prop_HeytingAlgebra @(Bool))
    prop "HeytingAlgebra (Lifted Bool)" (prop_HeytingAlgebra @(HArbitrary Lifted Bool))
    prop "HeytingAlgebra (Dropped Bool" (prop_HeytingAlgebra @(HArbitrary Dropped Bool))
    prop "HeytingAlgebra (Dropped (Lifted Bool))" (prop_HeytingAlgebra @(Composed Dropped Lifted Bool))
    prop "HeytingAlgebra (Lifted (Dropped Bool))" (prop_HeytingAlgebra @(Composed Lifted Dropped Bool))
    prop "HeytingAlgebra (Lifted (Lifted Bool))" (prop_HeytingAlgebra @(Composed Lifted Lifted Bool))
    prop "HeytingAlgebra (Dropped (Dropped Bool))" (prop_HeytingAlgebra @(Composed Dropped Dropped Bool))
