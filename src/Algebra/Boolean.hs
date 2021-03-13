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
  , runBooleanAlgebra
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

class Heyting a => Boolean a

instance Boolean Bool

instance Boolean All

instance Boolean Any

instance Boolean ()

instance Boolean (Proxy a)

instance Boolean a => Boolean (Tagged t a)

instance Boolean b => Boolean (a -> b)

#if MIN_VERSION_base(4,8,0)
instance Boolean a => Boolean (Identity a)
#endif

instance Boolean a => Boolean (Const a b)

instance Boolean a => Boolean (Endo a)

-- | Every Heyting algebra contains a Boolean algebra. @'toBoolean'@ maps onto
-- it; moreover it is a monad (Heyting algebra is a category as every poset is)
-- which preserves finite infima.
--
toBoolean :: Heyting a => a -> a
toBoolean = neg . neg

-- |
-- @'Boolean'@ is the left adjoint functor from the category of Heyting algebras
-- to the category of Boolean algebras; its right adjoint is the inclusion.
newtype BooleanAlgebra a = BooleanAlgebra
    { runBooleanAlgebra :: a -- ^ extract value from @'Boolean'@
    }
  deriving
    ( BoundedJoinSemiLattice
    , BoundedMeetSemiLattice
    , Lattice
    , Heyting
    , Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic
    )

instance Heyting a => Boolean (BooleanAlgebra a)

-- |
-- Smart constructro of the @'Boolean'@ type.
boolean :: Heyting a => a -> BooleanAlgebra a
boolean = BooleanAlgebra . toBoolean

