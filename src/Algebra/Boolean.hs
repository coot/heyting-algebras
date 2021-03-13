{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Boolean
  ( BooleanAlgebra
  , (==>)
  , neg
    -- * Adjunction between Boolean and Heyting algebras
  , Boolean
  , runBoolean
  , boolean
  ) where

import           Control.Applicative    (Const (..))
import           Data.Data              (Data, Typeable)
import           Data.Functor.Identity  (Identity (..))
import           Data.Proxy             (Proxy (..))
import           Data.Semigroup         ( All (..)
                                        , Any (..)
                                        , Endo (..)
                                        )
import           Data.Tagged            (Tagged (..))
import           GHC.Generics           (Generic)

import           Algebra.Heyting
import           Algebra.Lattice        ( Lattice
                                        , BoundedJoinSemiLattice
                                        , BoundedMeetSemiLattice
                                        )

class Heyting a => BooleanAlgebra a

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

-- | Every Heyting algebra contains a Boolean algebra. @'toBoolean'@ maps onto
-- it; moreover it is a monad (Heyting algebra is a category as every poset is)
-- which preserves finite infima.
--
toBoolean :: Heyting a => a -> a
toBoolean = neg . neg

-- |
-- @'Boolean'@ is the left adjoint functor from the category of Heyting algebras
-- to the category of Boolean algebras; its right adjoint is the inclusion.
newtype Boolean a = Boolean
    { runBoolean :: a -- ^ extract value from @'Boolean'@
    }
  deriving
    ( BoundedJoinSemiLattice
    , BoundedMeetSemiLattice
    , Lattice
    , Heyting
    , Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic
    )

instance Heyting a => BooleanAlgebra (Boolean a)

-- |
-- Smart constructro of the @'Boolean'@ type.
boolean :: Heyting a => a -> Boolean a
boolean = Boolean . toBoolean

