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

    -- * Properties
  , prop_not 
  , prop_BooleanAlgebra
  ) where

import Prelude hiding (not)

import Control.Applicative   (Const (..))
import Data.Data             (Data, Typeable)
import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))
import Data.Semigroup        (Endo (..))
import GHC.Generics          (Generic)
import Test.QuickCheck hiding ((==>))

import Algebra.Lattice ( Lattice
                       , BoundedLattice
                       , JoinSemiLattice
                       , BoundedJoinSemiLattice
                       , MeetSemiLattice
                       , BoundedMeetSemiLattice
                       , bottom
                       , top
                       , (/\)
                       , (\/)
                       )

import Algebra.Heyting ( HeytingAlgebra (..)
                       , iff
                       , iff'
                       , not
                       , toBoolean
                       , prop_HeytingAlgebra
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

-- TODO: move to tests
instance (Arbitrary a, HeytingAlgebra a) => Arbitrary (Boolean a) where
  arbitrary = boolean <$> arbitrary
  shrink (Boolean a) = [ boolean a' | a' <- shrink a ]

-- |
-- Smart constructro of the @'Boolean'@ type.
boolean :: HeytingAlgebra a => a -> Boolean a
boolean = Boolean . toBoolean

--
-- Instances
--

instance BooleanAlgebra ()

instance BooleanAlgebra (Proxy a)

instance BooleanAlgebra Bool

instance BooleanAlgebra b => BooleanAlgebra (a -> b)

instance BooleanAlgebra a => BooleanAlgebra (Identity a)

instance BooleanAlgebra a => BooleanAlgebra (Const a b)

instance BooleanAlgebra a => BooleanAlgebra (Endo a)

instance (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (a, b)

-- 
-- Properties
--

prop_not :: (BooleanAlgebra a, Eq a, Show a) => a -> Property
prop_not a =
       not (not a) === a
  .&&. not a /\ a === bottom
  .&&. not a \/ a === top

prop_BooleanAlgebra :: (BooleanAlgebra a, Eq a, Show a)
                    => a -> a -> a -> Property
prop_BooleanAlgebra a b c =
       prop_HeytingAlgebra a b c
  .&&. prop_not a
