{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Universe.Class (Universe (..), Finite)
import qualified Data.Set as S
import qualified Data.Map as M

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Algebra.Boolean
import Algebra.Heyting

-- | Arbitrary wrapper
newtype Arb a = Arb a
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

instance Show a => Show (Arb a) where
  show (Arb a) = show a

instance (Finite k, Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Arb (M.Map k v)) where
  arbitrary = frequency 
    [ (1, Arb . M.fromList <$> arbitrary)
    , (4, Arb . M.fromList . zip universe <$> vectorOf (length (universe @k)) arbitrary)
    , (6, return $ Arb M.empty)
    ]

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

data S5 = S1 | S2 | S3 | S4 | S5
  deriving (Ord, Eq, Show)

instance Universe S5 where
  universe = [S1, S2, S3, S4, S5]

instance Finite S5

instance Arbitrary S5 where
  arbitrary = elements universe

instance (Arbitrary a, Ord a) => Arbitrary (Tagged S.Set a) where
  arbitrary = Tagged . S.fromList <$> arbitrary
  shrink (Tagged as) = [ Tagged (S.fromList as') | as' <- shrink (S.toList as) ]

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

    prop "BooleanAlgebra (Set S5)" (prop_BooleanAlgebra @(Tagged S.Set S5))
    prop "HeytingAlgebra (Map S5 Bool)" (prop_HeytingAlgebra @(Arb (M.Map S5 Bool)))

    prop "HeytingAlgebra (Dropped (Lifted Bool))" (prop_HeytingAlgebra @(Composed Dropped Lifted Bool))
    prop "HeytingAlgebra (Lifted (Dropped Bool))" (prop_HeytingAlgebra @(Composed Lifted Dropped Bool))
    prop "HeytingAlgebra (Lifted (Lifted Bool))" (prop_HeytingAlgebra @(Composed Lifted Lifted Bool))
    prop "HeytingAlgebra (Dropped (Dropped Bool))" (prop_HeytingAlgebra @(Composed Dropped Dropped Bool))
