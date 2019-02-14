module Algebra.Boolean.Properties where

import           Prelude hiding (not)

import           Algebra.Lattice (bottom, top, (/\), (\/))
import           Algebra.Boolean
import           Algebra.Heyting
import           Algebra.Heyting.CounterExample ( CounterExample
                                                , annotate
                                                , fmapCounterExample
                                                , (===)
                                                )
import           Algebra.Heyting.Properties

-- |
-- Test that @'not'@ satisfies Boolean algebra axioms.
prop_not :: (HeytingAlgebra a, Ord a, Eq a, Ord e) => a -> CounterExample e
prop_not a =
     (not (not a) === a)
  /\ (not a /\ a === bottom)
  /\ (not a \/ a === top)

data BooleanAlgebraLawViolation a
  = BALVHeytingAlgebraLawViolation (HeytingAlgebraLawViolation a)
  | BALVNotLawViolation a
  deriving (Eq, Ord, Show)

-- |
-- Test that @a@ is satisfy both @'Algebra.Heyting.prop_HeytingAlgebra'@ and
-- @'prop_not'@.
prop_BooleanAlgebra
  :: (BooleanAlgebra a, Ord a, Eq a, Show a)
  => a -> a -> a -> CounterExample (BooleanAlgebraLawViolation a)
prop_BooleanAlgebra a b c =
     (fmapCounterExample BALVHeytingAlgebraLawViolation $ prop_HeytingAlgebra a b c)
  /\ annotate (BALVNotLawViolation a) (prop_not a)
