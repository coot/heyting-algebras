{-# LANGUAGE TypeFamilies #-}
module Algebra.Boolean.Free
  ( FreeBoolean (..)
  ) where

import Control.Monad (ap)
import Algebra.Lattice
  ( BoundedJoinSemiLattice (..)
  , JoinSemiLattice (..)
  , BoundedMeetSemiLattice (..)
  , MeetSemiLattice (..)
  , BoundedLattice
  , Lattice
  )
import Data.Algebra.Free
  ( AlgebraType0
  , AlgebraType
  , FreeAlgebra (..)
  , Proof (..)
  , fmapFree
  , bindFree
  )
import Data.Constraint (Dict (..))

import Algebra.Boolean (BooleanAlgebra)
import Algebra.Heyting (HeytingAlgebra (..))

-- |
-- Free Boolean algebra.  @'FreeAlgebra'@ instance provides all the usual
-- combinators for a free algebra.
newtype FreeBoolean a = FreeBoolean
  { runFreeBoolean :: forall h . BooleanAlgebra h => (a -> h) -> h }

instance JoinSemiLattice (FreeBoolean a) where
  FreeBoolean f \/ FreeBoolean g = FreeBoolean (\inj -> f inj \/ g inj)

instance BoundedJoinSemiLattice (FreeBoolean a) where
  bottom = FreeBoolean (\_ -> bottom)

instance MeetSemiLattice (FreeBoolean a) where
  FreeBoolean f /\ FreeBoolean g = FreeBoolean (\inj -> f inj /\ g inj)

instance BoundedMeetSemiLattice (FreeBoolean a) where
  top = FreeBoolean (\_ -> top)

instance Lattice (FreeBoolean a)

instance BoundedLattice (FreeBoolean a)

instance HeytingAlgebra (FreeBoolean a) where
  FreeBoolean f ==> FreeBoolean g = FreeBoolean (\inj -> f inj ==> g inj)

instance BooleanAlgebra (FreeBoolean a)

type instance AlgebraType0 FreeBoolean a = ()
type instance AlgebraType  FreeBoolean a = BooleanAlgebra a
instance FreeAlgebra FreeBoolean where
  returnFree a = FreeBoolean (\inj -> inj a)
  foldMapFree f (FreeBoolean inj) = inj f

  proof  = Proof Dict
  forget = Proof Dict

instance Functor FreeBoolean where
  fmap = fmapFree

instance Applicative FreeBoolean where
  pure = returnFree
  (<*>) = ap

instance Monad FreeBoolean where
  return = returnFree
  (>>=) = bindFree
