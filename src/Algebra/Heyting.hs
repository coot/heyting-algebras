{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Heyting
  ( HeytingAlgebra (..)
  , toBool
  , HBool
  , hBool

    -- * Properties
  , prop_HeytingAlgebra
  , prop_implies
  )
  where

import Prelude hiding (not)

import Algebra.Lattice ( JoinSemiLattice
                       , BoundedJoinSemiLattice
                       , MeetSemiLattice
                       , BoundedMeetSemiLattice
                       , Lattice
                       , BoundedLattice
                       , Meet (..)
                       , bottom
                       , top
                       , meetLeq
                       , (/\)
                       )
import Algebra.Lattice.Dropped (Dropped (..))
import Algebra.Lattice.Lifted (Lifted (..))
import Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as L
import Algebra.PartialOrd (leq, partialOrdEq)
import Test.QuickCheck

import Algebra.Boolean ( BooleanAlgebra
                       , prop_BoundedJoinSemiLattice
                       , prop_BoundedMeetSemiLattice
                       )
import qualified Algebra.Boolean as B


class BoundedLattice a => HeytingAlgebra a where
  implies :: a -> a -> a

not :: HeytingAlgebra a => a -> a
not a = a `implies` bottom

iff :: HeytingAlgebra a => a -> a -> a
iff a b = (a `implies` b) /\ (b `implies` a)

iff' :: (Eq a, HeytingAlgebra a) => a -> a -> Bool
iff' a b = Meet top `leq` Meet (iff a b)

instance HeytingAlgebra Bool where
  implies = B.implies

instance HeytingAlgebra () where
  implies _ _ = ()

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
  implies f g = \a -> f a `implies` g a

instance (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (a, b) where
  implies (a0, b0) (a1, b1) = (a0 `implies` a1, b0 `implies` b1)

instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Dropped a) where
  implies (Drop a) (Drop b) | Meet a `partialOrdEq` Meet b = Top
                            | otherwise                    = Drop (a `implies` b)
  implies Top      a        = a
  implies _        Top      = Top

instance HeytingAlgebra a => HeytingAlgebra (Lifted a) where
  implies (Lift a) (Lift b) = Lift (a `implies` b)
  implies Bottom   _        = Lift top
  implies _        Bottom   = Bottom

instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Levitated a) where
  implies (L.Levitate a) (L.Levitate b) | Meet a `partialOrdEq` Meet b
                                        = L.Top
                                        | otherwise
                                        = L.Levitate (a `implies` b)
  implies L.Top          a              = a
  implies _              L.Top          = L.Top
  implies L.Bottom       _              = L.Top
  implies _              L.Bottom       = L.Bottom

-- |
-- Every Heyting algebra contains a Boolean algebra. @'toBool'@ maps onto it;
-- moreover its a monad (Heyting algebra is a category as every poset is) which
-- preserves finite infima.
toBool :: HeytingAlgebra a => a -> a
toBool = not . not

-- |
-- @'HBool'@ is the left adjoint functor from the category of Heyting algebras
-- to the category of Boolean algebra; its right adjoint is the inclusion
-- (every Boolean algebra is a Heyting algebra).
newtype HBool a = HBool a
  deriving ( JoinSemiLattice
           , BoundedJoinSemiLattice
           , MeetSemiLattice
           , BoundedMeetSemiLattice
           , Lattice
           , BoundedLattice
           , Eq
           , Ord
           , Show
           )

hBool :: HeytingAlgebra a => a -> HBool a
hBool = HBool . toBool

instance HeytingAlgebra a => BooleanAlgebra (HBool a) where
  not (HBool a) = HBool (not a)

instance (Arbitrary a, HeytingAlgebra a) => Arbitrary (HBool a) where
  arbitrary = hBool <$> arbitrary

--
-- Properties
--

-- For all `a`: `_ /\ a` is left adjoint to  `implies a`, i.e.
-- prop> Boolean (x /\ a `meetLeq` y) `iff'` Boolean (x `meetLeq` (a `implies` b))
prop_implies :: (HeytingAlgebra a, Eq a, Show a)
             => a -> a -> Property
prop_implies a b =
       counterexample
        (show a ++ " ⇒ " ++ show b ++ " /\\ " ++ show a ++ " NOT ≤ " ++ show b)
        ((a `implies` b /\ a) `meetLeq` b)
  .&&. counterexample
        (show b ++ " NOT ≤ " ++ show a ++ " ⇒ " ++ show a ++ " /\\ " ++ show b)
        (b `meetLeq` (a `implies` a /\ b))

-- |
-- Usefull for testing valid instances of `HeytingAlgebra` type class.
prop_HeytingAlgebra :: (HeytingAlgebra a, Eq a, Show a)
                    => a -> a -> a -> Property
prop_HeytingAlgebra a b c = 
       prop_BoundedJoinSemiLattice a b c
  .&&. prop_BoundedMeetSemiLattice a b c
  .&&. prop_implies a b
