{-# LANGUAGE TypeFamilies #-}
module Algebra.Boolean.Free
  ( FreeBoolean (..)
  ) where

import           Control.Monad     (ap)
import           Algebra.Lattice   ( BoundedJoinSemiLattice (..)
                                   , BoundedMeetSemiLattice (..)
                                   , Lattice (..)
                                   )
import           Data.Algebra.Free ( AlgebraType0
                                   , AlgebraType
                                   , FreeAlgebra (..)
                                   , fmapFree
                                   , bindFree
                                   )

import           Algebra.Boolean   (Boolean)
import           Algebra.Heyting

-- |
-- Free Boolean algebra.  @'FreeAlgebra'@ instance provides all the usual
-- combinators for a free algebra.
newtype FreeBoolean a = FreeBoolean
  { runFreeBoolean :: forall h . Boolean h => (a -> h) -> h }

instance BoundedJoinSemiLattice (FreeBoolean a) where
  bottom = FreeBoolean (\_ -> bottom)

instance BoundedMeetSemiLattice (FreeBoolean a) where
  top = FreeBoolean (\_ -> top)

instance Lattice (FreeBoolean a) where
  FreeBoolean f /\ FreeBoolean g = FreeBoolean (\inj -> f inj /\ g inj)
  FreeBoolean f \/ FreeBoolean g = FreeBoolean (\inj -> f inj \/ g inj)

instance Heyting (FreeBoolean a) where
  FreeBoolean f ==> FreeBoolean g = FreeBoolean (\inj -> f inj ==> g inj)

instance Boolean (FreeBoolean a)

type instance AlgebraType0 FreeBoolean a = ()
type instance AlgebraType  FreeBoolean a = Boolean a
instance FreeAlgebra FreeBoolean where
  returnFree a = FreeBoolean (\inj -> inj a)
  foldMapFree f (FreeBoolean inj) = inj f

instance Functor FreeBoolean where
  fmap = fmapFree

instance Applicative FreeBoolean where
  pure = returnFree
  (<*>) = ap

instance Monad FreeBoolean where
  return = returnFree
  (>>=) = bindFree
