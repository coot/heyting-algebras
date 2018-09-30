module Algebra.Boolean
  ( BooleanAlgebra (..)
  , implies
  ) where

import Prelude hiding (not, (||), (&&))
import qualified Prelude

class BooleanAlgebra a where
  ff :: a
  tt :: a

  (||) :: a -> a -> a
  (&&) :: a -> a -> a

  not :: a -> a

implies :: BooleanAlgebra a => a -> a -> a
implies a b = not a || b

infixr 3 &&
infixr 2 ||

instance BooleanAlgebra Bool where
  ff = False
  tt = True
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  not = Prelude.not

instance (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (a, b) where
  ff = (ff, ff)
  tt = (tt, tt)
  (a0, b0) && (a1, b1) = (a0 && a1, b0 && b1)
  (a0, b0) || (a1, b1) = (a0 || a1, b0 || b1)
  not (a0, b0) = (not a0, not b0)
