{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PackageImports             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.Universe.Class (Universe (..), Finite)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map

import           Algebra.Lattice ( BoundedJoinSemiLattice
                                 , BoundedMeetSemiLattice
                                 , Lattice (..)
                                 , Meet (..)
                                 , bottom
                                 , top
                                 )
import           Algebra.Lattice.Dropped (Dropped (..))
import           Algebra.Lattice.Lifted (Lifted (..))
import           Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as L
import           Algebra.Lattice.Op (Op (..))
import           Algebra.Lattice.Ordered (Ordered (..))

import           Algebra.Boolean
import           Algebra.Boolean.Properties
import           Algebra.Heyting
import           Algebra.Heyting.CounterExample
import           Algebra.Heyting.Properties
import           Algebra.Heyting.Layered
import           Algebra.PartialOrd (leq)

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Ordered, (==>))

main :: IO ()
main = defaultMain tests

counterExampleProperty
  :: Show e
  => CounterExample e
  -> Property
counterExampleProperty = maybe (property True) (flip counterexample False) . fromCounterExample'

--
-- Orphan instances
-- issue: https://github.com/haskellari/lattices/pull/112
--

instance Heyting a => Heyting (Lifted a) where
  (Lift a) ==> (Lift b) = Lift (a ==> b)
  Bottom   ==> _        = Lift top
  _        ==> Bottom   = Bottom

instance (Eq a, Heyting a) => Heyting (Dropped a) where
  (Drop a) ==> (Drop b) | Meet a `leq` Meet b = Top
                        | otherwise           = Drop (a ==> b)
  Top      ==> a        = a
  _        ==> Top      = Top

instance BooleanAlgebra a => Heyting (Op a) where
  (Op a) ==> (Op b) = Op (neg a /\ b)

instance (Eq a, Heyting a) => Heyting (Levitated a) where
  (L.Levitate a) ==> (L.Levitate b) | Meet a `leq` Meet b = L.Top
                                    | otherwise           = L.Levitate (a ==> b)
  L.Top          ==> a              = a
  _              ==> L.Top          = L.Top
  L.Bottom       ==> _              = L.Top
  _              ==> L.Bottom       = L.Bottom

-- 
-- List of tast cases
--

tests :: TestTree
tests =
  testGroup "heyting-algebras tests"
    [ testGroup "Boolean algebras"
        [ testProperty "Bool"                                         prop_boolean_Bool
        , testProperty "Boolean (Lifted Bool)"                        prop_boolean_LiftedBool
        , testProperty "Boolean (Dropped Bool)"                       prop_boolean_DroppedBool
        , testProperty "(Set S5)"                                     prop_boolean_Set
        ]
    , testGroup "Non Boolean algebras"
        [ testProperty "Not a BooleanAlgebra (Lifted Bool)"           prop_non_boolean_LiftedBool
        , testProperty "Not a BooleanAlgebra (Dropped Bool)"          prop_non_boolean_DroppedBool
        , testProperty "Not a BooleanAlgebra Levitated (Ordered Int)" prop_non_boolean_LevitatedOrderedInt
        ]
    , testGroup "Heyting algebras"
        [ testProperty "Lifted Bool"                                  prop_heyting_LiftedBool
        , testProperty "Dropped Bool"                                 prop_heyting_DroppedBool
        , testProperty "Layered Bool Bool"                            prop_heyting_LayeredBoolBool
        , testProperty "Levitated Bool"                               prop_heyting_LevitatedBool
        , testProperty "Sum (Lifted Bool) (Dropped Bool)"             prop_heyting_LayeredLiftedDropped
        , testProperty "Levitated (Ordered Int)"                      prop_heyting_LevitatedOrderedInt
        , testProperty "Dropped (Lifted Bool)"                        prop_heyting_DroppedLiftedBool
        , testProperty "Lifted (Dropped Bool)"                        prop_heyting_LiftedDroppedBool
        , testProperty "Lifted (Lifted Bool)"                         prop_heyting_LiftedLiftedBool
        , testProperty "Dropped (Dropped Bool)"                       prop_heyting_DroppedDroppedBool
        ]
    ]

--
-- Boolean algebra tests
--

type BooleanProp a = a -> a -> a -> Property

prop_boolean_Bool
  :: BooleanProp Bool
prop_boolean_Bool =
  (fmap . fmap) counterExampleProperty . prop_BooleanAlgebra

prop_boolean_LiftedBool
  :: BooleanProp (Arb (Boolean (Arb (Lifted Bool))))
prop_boolean_LiftedBool =
  (fmap . fmap) counterExampleProperty . prop_BooleanAlgebra

prop_boolean_DroppedBool
  :: BooleanProp (Arb (Boolean (Arb (Dropped Bool))))
prop_boolean_DroppedBool =
  (fmap . fmap) counterExampleProperty . prop_BooleanAlgebra

prop_boolean_Set
  :: BooleanProp (Arb (Set S5))
prop_boolean_Set =
  (fmap . fmap) counterExampleProperty . prop_BooleanAlgebra

--
-- Non Boolean algebra tests
--

type NonBooleanProp a = a -> Property

prop_non_boolean_LiftedBool
  :: NonBooleanProp (Arb (Lifted Bool))
prop_non_boolean_LiftedBool =
    expectFailure
  . counterExampleProperty @String
  . prop_neg

prop_non_boolean_DroppedBool
  :: NonBooleanProp (Arb (Dropped Bool))
prop_non_boolean_DroppedBool =
    expectFailure
  . counterExampleProperty @String
  . prop_neg

prop_non_boolean_LevitatedOrderedInt
  :: NonBooleanProp (Arb (Levitated (Arb (Ordered Int))))
prop_non_boolean_LevitatedOrderedInt =
    expectFailure
  . counterExampleProperty @String
  . prop_neg

--
-- Heyting algebra tests
--

type HeytingProp a = a -> a -> a -> Property

prop_heyting_LiftedBool
  :: HeytingProp (Arb (Lifted Bool))
prop_heyting_LiftedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_DroppedBool
  :: HeytingProp (Arb (Dropped Bool))
prop_heyting_DroppedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LayeredBoolBool
  :: HeytingProp (Arb (Layered Bool Bool))
prop_heyting_LayeredBoolBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LevitatedBool
  :: HeytingProp (Arb (Levitated Bool))
prop_heyting_LevitatedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LayeredLiftedDropped
  :: HeytingProp (Arb (Layered (Arb (Lifted Bool)) (Arb (Dropped Bool))))
prop_heyting_LayeredLiftedDropped =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LevitatedOrderedInt
  :: HeytingProp (Arb (Levitated (Arb (Ordered Int))))
prop_heyting_LevitatedOrderedInt =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_DroppedLiftedBool
  :: HeytingProp (Composed Dropped Lifted Bool)
prop_heyting_DroppedLiftedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LiftedDroppedBool
  :: HeytingProp (Composed Lifted Dropped Bool)
prop_heyting_LiftedDroppedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_LiftedLiftedBool
  :: HeytingProp (Composed Lifted Lifted Bool)
prop_heyting_LiftedLiftedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra

prop_heyting_DroppedDroppedBool
  :: HeytingProp (Composed Dropped Dropped Bool)
prop_heyting_DroppedDroppedBool =
  (fmap . fmap) counterExampleProperty . prop_HeytingAlgebra


-- | Arbitrary wrapper for varous lattices.
--
newtype Arb a = Arb a
  deriving ( BoundedJoinSemiLattice
           , BoundedMeetSemiLattice
           , Lattice
           , BooleanAlgebra
           , Heyting
           , Eq
           , Ord
           )

instance Show a => Show (Arb a) where
  show (Arb a) = show a

instance (Finite k, Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Arb (Map k v)) where
  arbitrary = frequency 
    [ (1, return $ Arb Map.empty)
    , (1, Arb . Map.fromList . zip universe <$> vectorOf (length (universe @k)) arbitrary)
    , (8, Arb . Map.fromList <$> arbitrary)
    ]

instance Arbitrary a => Arbitrary (Arb (Lifted a)) where
  arbitrary = Arb . maybe Bottom Lift <$> arbitrary
  shrink (Arb Bottom)   = []
  shrink (Arb (Lift a)) =
    Arb Bottom : (Arb . Lift <$> shrink a)

instance Arbitrary a => Arbitrary (Arb (Dropped a)) where
  arbitrary = Arb . maybe Top Drop <$> arbitrary
  shrink (Arb Top)   = []
  shrink (Arb (Drop a)) =
    Arb Top : (Arb . Drop <$> shrink a)

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

instance (Arbitrary a, Heyting a, Eq a) => Arbitrary (Arb (Boolean a)) where
  arbitrary = Arb . boolean <$> arbitrary
  shrink (Arb a) = filter (/= Arb a) (Arb . boolean <$> shrink (runBoolean a))

 
instance Arbitrary a => Arbitrary (Arb (Ordered a)) where
  arbitrary = Arb . Ordered <$> arbitrary
  shrink (Arb (Ordered a)) = Arb . Ordered <$> shrink a

data S5 = S1 | S2 | S3 | S4 | S5
  deriving (Ord, Eq, Show)

instance Universe S5 where
  universe = [S1, S2, S3, S4, S5]

instance Finite S5

instance Arbitrary S5 where
  arbitrary = elements universe

instance (Arbitrary a, Ord a) => Arbitrary (Arb (Set a)) where
  arbitrary       = Arb . Set.fromList <$> arbitrary
  shrink (Arb as) = [ Arb (Set.fromList as') | as' <- shrink (Set.toList as) ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Arb (Layered a b)) where
  arbitrary = oneof
    [ Arb . Lower <$> arbitrary
    , Arb . Upper <$> arbitrary
    ]
  shrink (Arb (Lower a)) = Arb . Lower <$> shrink a
  shrink (Arb (Upper b)) = Arb . Upper <$> shrink b

-- | Arbitrary newtype wrapper for compositions of heigher kinded types.
--
newtype Composed f g a = Composed (f (g a))
  deriving ( BoundedJoinSemiLattice
           , BoundedMeetSemiLattice
           , Lattice
           , BooleanAlgebra
           , Heyting
           , Eq
           , Ord
           )

instance Show (f (g a)) => Show (Composed f g a) where
  show (Composed fga) = show fga

instance Arbitrary a => Arbitrary (Composed Dropped Lifted a) where
  arbitrary = frequency
    [ (1, return $ Composed Top)
    , (1, return $ Composed (Drop Bottom))
    , (8, Composed . Drop . Lift  <$> arbitrary)
    ]

  shrink (Composed Top)             = []
  shrink (Composed (Drop Bottom))   = [Composed Top]
  shrink (Composed (Drop (Lift a))) =
       [ Composed (Drop Bottom) ]
    ++ [ Composed (Drop (Lift a')) | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Composed Lifted Dropped a) where
  arbitrary = frequency
    [ (1, return $ Composed Bottom)
    , (1, return $ Composed (Lift Top))
    , (8, Composed . Lift . Drop  <$> arbitrary)
    ]

instance Arbitrary a => Arbitrary (Composed Lifted Lifted a) where
  arbitrary = frequency
    [ (1, return $ Composed Bottom)
    , (1, return $ Composed (Lift Bottom))
    , (8, Composed . Lift . Lift  <$> arbitrary)
    ]

  shrink (Composed Bottom)          = []
  shrink (Composed (Lift Bottom))   = [Composed Bottom]
  shrink (Composed (Lift (Lift a))) =
       [ Composed (Lift Bottom) ]
    ++ [ Composed (Lift (Lift a')) | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Composed Dropped Dropped a) where
  arbitrary = frequency
    [ (1, return $ Composed Top)
    , (1, return $ Composed (Drop Top))
    , (8, Composed . Drop . Drop  <$> arbitrary)
    ]

  shrink (Composed Top)          = []
  shrink (Composed (Drop Top))   = [Composed Top]
  shrink (Composed (Drop (Drop a))) =
       [ Composed Top, Composed (Drop Top) ]
    ++ [ Composed (Drop (Drop a')) | a' <- shrink a ]

instance Arbitrary a => Arbitrary (Composed Lifted Op a) where
  arbitrary = frequency
    [ (1, return (Composed Bottom))
    , (9, Composed . Lift . Op <$> arbitrary)
    ]
