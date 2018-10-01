module Algebra.Boolean
  ( BooleanAlgebra (..)
  , implies
  , iff
  , iff'

    -- Properties
  , prop_BoundedJoinSemiLattice
  , prop_BoundedMeetSemiLattice
  , prop_not 
  , prop_BooleanAlgebra
  ) where

import Prelude hiding (not)
import qualified Prelude

import Control.Applicative   (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))
import Data.Semigroup        (Endo (..))
import Test.QuickCheck

import Algebra.Lattice ( BoundedLattice
                       , BoundedJoinSemiLattice
                       , BoundedMeetSemiLattice
                       , Meet (..)
                       , bottom
                       , top
                       , meetLeq
                       , joinLeq
                       , (/\)
                       , (\/)
                       )
import Algebra.PartialOrd (leq)

class BoundedLattice a => BooleanAlgebra a where
  not :: a -> a

implies :: BooleanAlgebra a => a -> a -> a
implies a b = not a \/ b

iff :: BooleanAlgebra a => a -> a -> a
iff a b = (a `implies` b) /\ (b `implies` a)

iff' :: (Eq a, BooleanAlgebra a) => a -> a -> Bool
iff' a b = Meet top `leq` Meet (iff a b)

instance BooleanAlgebra () where
  not _ = ()

instance BooleanAlgebra (Proxy a) where
  not _ = Proxy

instance BooleanAlgebra Bool where
  not = Prelude.not

instance BooleanAlgebra b => BooleanAlgebra (a -> b) where
  not f = not . f

instance BooleanAlgebra a => BooleanAlgebra (Identity a) where
  not (Identity a) = Identity (not a)

instance BooleanAlgebra a => BooleanAlgebra (Const a b) where
  not (Const a) = Const (not a)

instance BooleanAlgebra a => BooleanAlgebra (Endo a) where
  not (Endo f) = Endo (not . f)

instance (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (a, b) where
  not (a0, b0) = (not a0, not b0)

prop_BoundedMeetSemiLattice :: (BoundedMeetSemiLattice a, Eq a, Show a)
                            => a -> a -> a -> Property
prop_BoundedMeetSemiLattice a b c =
       counterexample "meet associativity" ((a /\ (b /\ c)) === ((a /\ b) /\ c))
  .&&. counterexample "meet commutativity" ((a /\ b) === (b /\ a))
  .&&. counterexample "meet idempotent" ((a /\ a) === a)
  .&&. counterexample "meet identity" ((top /\ a) === a)
  .&&. counterexample "meet order" ((a /\ b) `meetLeq` a)

prop_BoundedJoinSemiLattice :: (BoundedJoinSemiLattice a, Eq a, Show a) => a -> a -> a -> Property
prop_BoundedJoinSemiLattice a b c =
       counterexample "join associativity" ((a \/ (b \/ c)) === ((a \/ b) \/ c))
  .&&. counterexample "join commutativity" ((a \/ b) === (b \/ a))
  .&&. counterexample "join idempotent" ((a \/ a) === a)
  .&&. counterexample "join identity" ((bottom \/ a) === a)
  .&&. counterexample "join order" (a `joinLeq` (a \/ b))

prop_not :: (BooleanAlgebra a, Eq a, Show a) => a -> Property
prop_not a =
       not (not a) === a
  .&&. not a /\ a === bottom
  .&&. not a \/ a === top

prop_BooleanAlgebra :: (BooleanAlgebra a, Eq a, Show a)
                    => a -> a -> a -> Property
prop_BooleanAlgebra a b c =
       prop_BoundedJoinSemiLattice a b c
  .&&. prop_BoundedMeetSemiLattice a b c
  .&&. prop_not a
