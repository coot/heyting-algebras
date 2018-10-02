{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Heyting
  ( HeytingAlgebra (..)
  , iff
  , iff'
  , toBoolean

    -- * Properties
  , prop_BoundedMeetSemiLattice
  , prop_BoundedJoinSemiLattice
  , prop_HeytingAlgebra
  , prop_implies
  )
  where

import Prelude hiding (not)
import qualified Prelude

import Control.Applicative   (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))
import Data.Semigroup        (Endo (..))
import Data.Universe.Class   (Finite, universe)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge
import qualified Data.Set as S

import Algebra.Lattice ( BoundedJoinSemiLattice
                       , BoundedMeetSemiLattice
                       , BoundedLattice
                       , Meet (..)
                       , bottom
                       , top
                       , meetLeq
                       , joinLeq
                       , (/\)
                       , (\/)
                       )
import Algebra.Lattice.Dropped (Dropped (..))
import Algebra.Lattice.Lifted (Lifted (..))
import Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as L
import Algebra.PartialOrd (leq, partialOrdEq)
import Test.QuickCheck hiding ((==>))

-- |
-- Heyting algebra is a bounded semi-lattice with implication which should
-- satisfy the following law:
--
-- prop> x ∧ a ≤ y ⇔ x ≤ (a ⇒ b)
--
-- This means @a ⇒ b@ is an
-- [exponential object](https://ncatlab.org/nlab/show/exponential%2Bobject),
-- which makes any Heyting algebra
-- a [cartesian
-- closed category](https://ncatlab.org/nlab/show/cartesian%2Bclosed%2Bcategory).
class BoundedLattice a => HeytingAlgebra a where
  (==>) :: a -> a -> a
  (==>) a b = not a \/ b

  -- |
  -- Default implementation is
  --
  -- @
  -- not a = a '==>' 'bottom'
  -- @
  --
  -- It is useful for optimisation reasons.  Note that if you provide a valid
  -- Boolean @'not'@ the default @'==>'@ will make it into
  -- @'Algebra.Boolean.BooleanAlgebra'@.
  not :: a -> a
  not a = a ==> bottom

-- |
-- Less than fixity of both @'\/'@ and @'/\'@.
infixr 4 ==>

iff :: HeytingAlgebra a => a -> a -> a
iff a b = (a ==> b) /\ (b ==> a)

iff' :: (Eq a, HeytingAlgebra a) => a -> a -> Bool
iff' a b = Meet top `leq` Meet (iff a b)

instance HeytingAlgebra Bool where
  a ==> b = Prelude.not a \/ b
  not     = Prelude.not

instance HeytingAlgebra () where
  _ ==> _ = ()

instance HeytingAlgebra (Proxy a) where
  _ ==> _ = Proxy

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
  f ==> g = \a -> f a ==> g a

instance HeytingAlgebra a => HeytingAlgebra (Identity a) where
  (Identity a) ==> (Identity b) = Identity (a ==> b)

instance HeytingAlgebra a => HeytingAlgebra (Const a b) where
  (Const a) ==> (Const b) = Const (a ==> b)

instance HeytingAlgebra a => HeytingAlgebra (Endo a) where
  (Endo f) ==> (Endo g) = Endo (f ==> g)

instance (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (a, b) where
  (a0, b0) ==> (a1, b1) = (a0 ==> a1, b0 ==> b1)

instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Dropped a) where
  (Drop a) ==> (Drop b) | Meet a `partialOrdEq` Meet b = Top
                        | otherwise                    = Drop (a ==> b)
  Top      ==> a        = a
  _        ==> Top      = Top

instance HeytingAlgebra a => HeytingAlgebra (Lifted a) where
  (Lift a) ==> (Lift b) = Lift (a ==> b)
  Bottom   ==> _        = Lift top
  _        ==> Bottom   = Bottom

instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Levitated a) where
  (L.Levitate a) ==> (L.Levitate b) | Meet a `partialOrdEq` Meet b
                                    = L.Top
                                    | otherwise
                                    = L.Levitate (a ==> b)
  L.Top          ==> a              = a
  _              ==> L.Top          = L.Top
  L.Bottom       ==> _              = L.Top
  _              ==> L.Bottom       = L.Bottom

--
-- containers
--

-- |
-- Power set: the cannoical example of a Boolean algebra
instance (Ord a, Finite a) => HeytingAlgebra (S.Set a) where
  not a = S.fromList universe `S.difference` a

-- |
-- It is not a boolean algebra (even if the keys are).
instance (Ord k, Finite k, HeytingAlgebra v) => HeytingAlgebra (M.Map k v) where
  -- _xx__
  -- __yy_
  -- T_iyT where i = x ==> y; T = top
  a ==> b =
    Merge.merge
      Merge.dropMissing     -- drop @x@ if it is missing in @x@
      Merge.preserveMissing -- preserve @y@ if it is missing @x@
      (Merge.zipWithMatched (\_ -> (==>))) a b
                            -- merge with (==>) matching elements
    \/ M.fromList [(k, top) | k <- universe, not (M.member k a), not (M.member k b) ] 
                            -- for elements which are not in a, nor in b add
                            -- a @top@ key
  

-- |
-- Every Heyting algebra contains a Boolean algebra. @'toBoolean'@ maps onto it;
-- moreover it is a monad (Heyting algebra is a category as every poset is) which
-- preserves finite infima.
toBoolean :: HeytingAlgebra a => a -> a
toBoolean = not . not

--
-- Properties
--

-- |
-- Verfifies if bounded meet semilattice laws.
prop_BoundedMeetSemiLattice :: (BoundedMeetSemiLattice a, Eq a, Show a)
                            => a -> a -> a -> Property
prop_BoundedMeetSemiLattice a b c =
       counterexample "meet associativity" ((a /\ (b /\ c)) === ((a /\ b) /\ c))
  .&&. counterexample "meet commutativity" ((a /\ b) === (b /\ a))
  .&&. counterexample "meet idempotent" ((a /\ a) === a)
  .&&. counterexample "meet identity" ((top /\ a) === a)
  .&&. counterexample "meet order" ((a /\ b) `meetLeq` a)

-- |
-- Verfifies if bounded join semilattice laws.
prop_BoundedJoinSemiLattice :: (BoundedJoinSemiLattice a, Eq a, Show a) => a -> a -> a -> Property
prop_BoundedJoinSemiLattice a b c =
       counterexample "join associativity" ((a \/ (b \/ c)) === ((a \/ b) \/ c))
  .&&. counterexample "join commutativity" ((a \/ b) === (b \/ a))
  .&&. counterexample "join idempotent" ((a \/ a) === a)
  .&&. counterexample "join identity" ((bottom \/ a) === a)
  .&&. counterexample "join order" (a `joinLeq` (a \/ b))

-- |
-- For all @a@: @_ /\ a@ is left adjoint to  @==> a@, i.e.  But rather than:
--
-- prop> Boolean (x /\ a meetLeq y) iff' Boolean (x meetLeq (a '==>' b))
--
-- it checks the equivalent:
--
-- prop> ((a ==> b) /\ a) meetLeq b
-- prop> b meetLeq (a ==> a /\ b)
prop_implies :: (HeytingAlgebra a, Eq a, Show a)
             => a -> a -> Property
prop_implies a b =
       counterexample
        ("(a ⇒ b) ∧ a NOT ≤ b"
        ++ "\n a ⇒ b " ++ show (a ==> b))
        (((a ==> b) /\ a) `meetLeq` b)
  .&&. counterexample
        ("b NOT ≤ (a ⇒ a ∧ b)"
        ++ "\n a ⇒ a ∧ b " ++ show (a ==> a /\ b))
        (b `meetLeq` (a ==> a /\ b))

-- |
-- Usefull for testing valid instances of @'HeytingAlgebra'@ type class. It validates:
--
-- * bounded lattice laws
-- * @'prop_implies'@
prop_HeytingAlgebra :: (HeytingAlgebra a, Eq a, Show a)
                    => a -> a -> a -> Property
prop_HeytingAlgebra a b c = 
       prop_BoundedJoinSemiLattice a b c
  .&&. prop_BoundedMeetSemiLattice a b c
  .&&. prop_implies a b
