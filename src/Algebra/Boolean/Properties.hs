module Algebra.Boolean.Properties where

import           Prelude hiding (not)

import           Algebra.Lattice (bottom, top, (/\), (\/))
import           Algebra.Boolean
import           Algebra.Heyting
import           Algebra.Heyting.Properties

-- |
-- Test that @'not'@ satisfies Boolean algebra axioms.
prop_not :: (HeytingAlgebra a, Eq a, Show a) => a -> CounterExample
prop_not a =
     (not (not a) === a)
  /\ (not a /\ a === bottom)
  /\ (not a \/ a === top)

-- |
-- Test that @a@ is satisfy both @'Algebra.Heyting.prop_HeytingAlgebra'@ and
-- @'prop_not'@.
prop_BooleanAlgebra :: (BooleanAlgebra a, Eq a, Show a)
                    => a -> a -> a -> CounterExample
prop_BooleanAlgebra a b c =
     prop_HeytingAlgebra a b c
  /\ prop_not a
