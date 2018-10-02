{-# LANGUAGE TypeFamilies #-}
module Algebra.Heyting.Free
  ( FreeHeyting (..)
  ) where

import Prelude hiding (not)

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

import Algebra.Heyting (HeytingAlgebra (..))

-- |
-- Free Heyting algebra.  @'FreeAlgebra'@ instance provides all the usual
-- combinators for a free algebra.
newtype FreeHeyting a = FreeHeyting
  { runFreeHeyting :: forall h . HeytingAlgebra h => (a -> h) -> h }

instance JoinSemiLattice (FreeHeyting a) where
  FreeHeyting f \/ FreeHeyting g = FreeHeyting (\inj -> f inj \/ g inj)

instance BoundedJoinSemiLattice (FreeHeyting a) where
  bottom = FreeHeyting (\_ -> bottom)

instance MeetSemiLattice (FreeHeyting a) where
  FreeHeyting f /\ FreeHeyting g = FreeHeyting (\inj -> f inj /\ g inj)

instance BoundedMeetSemiLattice (FreeHeyting a) where
  top = FreeHeyting (\_ -> top)

instance Lattice (FreeHeyting a)

instance BoundedLattice (FreeHeyting a)

instance HeytingAlgebra (FreeHeyting a) where
  FreeHeyting f ==> FreeHeyting g = FreeHeyting (\inj -> f inj ==> g inj)
  not (FreeHeyting f)             = FreeHeyting (\inj -> not (f inj))

type instance AlgebraType0 FreeHeyting a = ()
type instance AlgebraType  FreeHeyting a = HeytingAlgebra a
instance FreeAlgebra FreeHeyting where
  returnFree a = FreeHeyting (\inj -> inj a)
  foldMapFree f (FreeHeyting inj) = inj f

  proof  = Proof Dict
  forget = Proof Dict

instance Functor FreeHeyting where
  fmap = fmapFree

instance Applicative FreeHeyting where
  pure = returnFree
  (<*>) = ap

instance Monad FreeHeyting where
  return = returnFree
  (>>=) = bindFree
