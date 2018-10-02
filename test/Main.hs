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
import Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as L
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Algebra.Boolean
import Algebra.Heyting

-- |
-- Tagged wrapper for Arbitrary instances
newtype Tagged t a = Tagged (t a)
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
           )

instance Show (t a) => Show (Tagged t a) where
  show (Tagged ta) = show ta

instance Arbitrary a => Arbitrary (Tagged Lifted a) where
  arbitrary = Tagged . maybe Bottom Lift <$> arbitrary
  shrink (Tagged Bottom)   = []
  shrink (Tagged (Lift a)) =
    Tagged Bottom : [ Tagged (Lift a') | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Tagged Dropped a) where
  arbitrary = Tagged . maybe Top Drop <$> arbitrary
  shrink (Tagged Top)   = []
  shrink (Tagged (Drop a)) =
    Tagged Top : [ Tagged (Drop a') | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Tagged Levitated a) where
  arbitrary = frequency
    [ (1, return $ Tagged L.Top)
    , (1, return $ Tagged L.Bottom)
    , (2, Tagged . L.Levitate <$> arbitrary)
    ]

  shrink (Tagged L.Bottom) = []
  shrink (Tagged (L.Levitate a))
    = Tagged L.Bottom
    : Tagged L.Top
    : [ Tagged (L.Levitate a') | a' <- shrink a ]
  shrink (Tagged L.Top) = []

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
           )

instance Show (f (g a)) => Show (Composed f g a) where
  show (Composed fga) = show fga

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
    prop "BooleanAlgebra Boolean (Lifted Bool)" (prop_BooleanAlgebra @(Boolean (Tagged Lifted Bool)))

    prop "HeytingAlgebra (Lifted Bool)" (prop_HeytingAlgebra @(Tagged Lifted Bool))
    prop "Not BooleanAlgebra (Lifted Bool)" (expectFailure $ prop_not @(Tagged Lifted Bool))

    prop "HeytingAlgebra (Dropped Bool)" (prop_HeytingAlgebra @(Tagged Dropped Bool))
    prop "Not BooleanAlgebra (Dropped Bool)" (expectFailure $ prop_not @(Tagged Dropped Bool))

    prop "HeytingAlgebra (Levitated Bool)" (prop_HeytingAlgebra @(Tagged Levitated Bool))
    prop "Not BooleanAlgebra (Levitated Bool)" (expectFailure $ prop_not @(Tagged Levitated Bool))

    prop "HeytingAlgebra (Dropped (Lifted Bool))" (prop_HeytingAlgebra @(Composed Dropped Lifted Bool))
    prop "HeytingAlgebra (Lifted (Dropped Bool))" (prop_HeytingAlgebra @(Composed Lifted Dropped Bool))
    prop "HeytingAlgebra (Lifted (Lifted Bool))" (prop_HeytingAlgebra @(Composed Lifted Lifted Bool))
    prop "HeytingAlgebra (Dropped (Dropped Bool))" (prop_HeytingAlgebra @(Composed Dropped Dropped Bool))
