{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Boolean
  ( BooleanAlgebra
  , (==>)
  , not
  , iff
  , iff'
    -- * Adjunction between Boolean and Heyting algebras
  , Boolean
  , runBoolean
  , boolean
  ) where

import           Prelude hiding (not)

import           Control.Applicative    (Const (..))
import           Data.Data              (Data, Typeable)
import           Data.Functor.Identity  (Identity (..))
import           Data.Proxy             (Proxy (..))
import           Data.Semigroup         (All (..), Any (..), Endo (..))
import           Data.Tagged            (Tagged (..))
import           Data.Universe.Class    (Finite)
import qualified Data.Set as S
import           GHC.Generics           (Generic)

import           Algebra.Lattice        ( Lattice
                                        , BoundedLattice
                                        , JoinSemiLattice (..)
                                        , BoundedJoinSemiLattice
                                        , MeetSemiLattice (..)
                                        , BoundedMeetSemiLattice
                                        )

import           Algebra.Heyting        ( HeytingAlgebra (..)
                                        , iff
                                        , iff'
                                        , not
                                        , toBoolean
                                        )

-- |
-- Boolean algebra is a Heyting algebra which negation satisfies the law of
-- excluded middle, i.e. either of the following:
--
-- prop> not . not == not
--
-- or
--
-- prop> x ∨ not x == top
--
-- Another characterisation of Boolean algebras is as
-- [complemented](https://en.wikipedia.org/wiki/Complemented_lattice)
-- [distributive lattices](https://ncatlab.org/nlab/show/distributive+lattice)
-- where the complement satisfies the following three properties:
--
-- prop> (not a) ∧ a == bottom and (not a) ∨ a == top -- excluded middle law
-- prop> not (not a) == a                             -- involution law
-- prop> a ≤ b ⇒ not b ≤ not a                        -- order-reversing
class HeytingAlgebra a => BooleanAlgebra a

-- |
-- @'Boolean'@ is the left adjoint functor from the category of Heyting algebras
-- to the category of Boolean algebras; its right adjoint is the inclusion.
newtype Boolean a = Boolean
    { runBoolean :: a -- ^ extract value from @'Boolean'@
    }
  deriving
    ( JoinSemiLattice, BoundedJoinSemiLattice, MeetSemiLattice
    , BoundedMeetSemiLattice, Lattice, BoundedLattice, HeytingAlgebra
    , Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic
    )

instance HeytingAlgebra a => BooleanAlgebra (Boolean a)

-- |
-- Smart constructro of the @'Boolean'@ type.
boolean :: HeytingAlgebra a => a -> Boolean a
boolean = Boolean . toBoolean

--
-- Instances
--

instance BooleanAlgebra Bool

instance BooleanAlgebra All

instance BooleanAlgebra Any

instance BooleanAlgebra ()

instance BooleanAlgebra (Proxy a)

instance BooleanAlgebra a => BooleanAlgebra (Tagged t a)

instance BooleanAlgebra b => BooleanAlgebra (a -> b)

#if MIN_VERSION_base(4,8,0)
instance BooleanAlgebra a => BooleanAlgebra (Identity a)
#endif

instance BooleanAlgebra a => BooleanAlgebra (Const a b)

instance BooleanAlgebra a => BooleanAlgebra (Endo a)

instance (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (a, b)

--
-- containers
--

instance (Ord a, Finite a) => BooleanAlgebra (S.Set a)
