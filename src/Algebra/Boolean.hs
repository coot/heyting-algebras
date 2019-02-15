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

import           Data.Data              (Data, Typeable)
import           GHC.Generics           (Generic)

import           Algebra.Lattice        ( Lattice
                                        , BoundedLattice
                                        , JoinSemiLattice (..)
                                        , BoundedJoinSemiLattice
                                        , MeetSemiLattice (..)
                                        , BoundedMeetSemiLattice
                                        )

import           Algebra.Heyting        ( HeytingAlgebra (..)
                                        , BooleanAlgebra
                                        , iff
                                        , iff'
                                        , not
                                        , toBoolean
                                        )

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

