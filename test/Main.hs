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

import Test.QuickCheck

import Algebra.Boolean
import Algebra.Heyting
import Algebra.Heyting.Layered

import Test.Tasty
import Test.Tasty.QuickCheck

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

instance Arbitrary a => Arbitrary (Arb (Lifted a)) where
  arbitrary = Arb . maybe Bottom Lift <$> arbitrary
  shrink (Arb Bottom)   = []
  shrink (Arb (Lift a)) =
    Arb Bottom : [ Arb (Lift a') | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Arb (Dropped a)) where
  arbitrary = Arb . maybe Top Drop <$> arbitrary
  shrink (Arb Top)   = []
  shrink (Arb (Drop a)) =
    Arb Top : [ Arb (Drop a') | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Arb (Levitated a)) where
  arbitrary = frequency
    [ (1, return $ Arb L.Top)
    , (1, return $ Arb L.Bottom)
    , (2, Arb . L.Levitate <$> arbitrary)
    ]

  shrink (Arb L.Bottom) = []
  shrink (Arb (L.Levitate a))
    = Arb L.Bottom
    : Arb L.Top
    : [ Arb (L.Levitate a') | a' <- shrink a ]
  shrink (Arb L.Top) = []

data S5 = S1 | S2 | S3 | S4 | S5
  deriving (Ord, Eq, Show)

instance Universe S5 where
  universe = [S1, S2, S3, S4, S5]

instance Finite S5

instance Arbitrary S5 where
  arbitrary = elements universe

instance (Arbitrary a, Ord a) => Arbitrary (Arb (S.Set a)) where
  arbitrary = Arb . S.fromList <$> arbitrary
  shrink (Arb as) = [ Arb (S.fromList as') | as' <- shrink (S.toList as) ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Arb (Layered a b)) where
  arbitrary = oneof
    [ Arb . Lower <$> arbitrary
    , Arb . Upper <$> arbitrary
    ]
  shrink (Arb (Lower a)) = [ Arb (Lower a') | a' <- shrink a ]
  shrink (Arb (Upper b)) = [ Arb (Upper b') | b' <- shrink b ]

-- Another arbitrary newtype wrapper; using tagged type let us avoid
-- overlapping instances.
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
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "heyting-algebras tests"
    [ testGroup "BooleanAlgebra tests"
        [ testProperty "Bool"                  $ prop_BooleanAlgebra @Bool
        , testProperty "(Bool, Bool)"          $ prop_BooleanAlgebra @(Bool, Bool)
        , testProperty "Boolean (Lifted Bool)" $ prop_BooleanAlgebra @(Boolean (Arb (Lifted Bool)))
        , testProperty "(Set S5)"              $ prop_BooleanAlgebra @(Arb (S.Set S5))
        ]
    , testGroup "Not BooleanAlgebra tests"
        [ testProperty "Not BooleanAlgebra (Lifted Bool)"    $ expectFailure $ prop_not @(Arb (Lifted Bool))
        , testProperty "Not BooleanAlgebra (Dropped Bool)"   $ expectFailure $ prop_not @(Arb (Dropped Bool))
        , testProperty "Not BooleanAlgebra (Levitated Bool)" $ expectFailure $ prop_not @(Arb (Levitated Bool))
        ]
    , testGroup "HeytingAlgebra tests"
        [ testProperty "Lifted Bool"            $ prop_HeytingAlgebra @(Arb (Lifted Bool))
        , testProperty "Dropped Bool"           $ prop_HeytingAlgebra @(Arb (Dropped Bool))
        , testProperty "Layered Bool Bool"      $ prop_HeytingAlgebra @(Arb (Layered Bool Bool))
        , testProperty "Levitated Bool"         $ prop_HeytingAlgebra @(Arb (Levitated Bool))
        , testProperty "Sum (Lifted Bool) (Dropped Bool)"
                                                $ prop_HeytingAlgebra @(Arb (Layered (Arb (Lifted Bool)) (Arb (Dropped Bool))))
        , testProperty "Map S5 Bool"            $ prop_HeytingAlgebra @(Arb (M.Map S5 Bool))
        , testProperty "Dropped (Lifted Bool)"  $ prop_HeytingAlgebra @(Composed Dropped Lifted Bool)
        , testProperty "Lifted (Dropped Bool)"  $ prop_HeytingAlgebra @(Composed Lifted Dropped Bool)
        , testProperty "Lifted (Lifted Bool)"   $ prop_HeytingAlgebra @(Composed Lifted Lifted Bool)
        , testProperty "Dropped (Dropped Bool)" $ prop_HeytingAlgebra @(Composed Dropped Dropped Bool)
        ]
    ]
