{-# LANGUAGE TupleSections #-}
-- |
-- Properties of Heyting algebras; useful for testing lawfulness of instances.
--
module Algebra.Heyting.Properties where

import           Prelude hiding (not)

import           Data.List (intersperse)
import           Data.Semigroup ((<>))

import           Algebra.Lattice ( Lattice
                                 , BoundedJoinSemiLattice
                                 , BoundedMeetSemiLattice
                                 , Meet (..)
                                 , Join (..)
                                 , top
                                 , bottom
                                 , (/\)
                                 , (\/)
                                 )
import           Algebra.PartialOrd (leq)
import           Algebra.Heyting
import           Algebra.Heyting.CounterExample

data BoundedMeetSemiLatticeLawViolation a
  = BMSLVNonAssociative a a a
  | BMSLVNonCommutative a a
  | BMSLVNonIdempotent a
  | BMSLVNonUnital a
  | BMSLVMeetOrderViolation a a
  deriving (Eq, Ord)

instance Show a => Show (BoundedMeetSemiLatticeLawViolation a) where
  show (BMSLVNonAssociative a b c) = withArgs "a ∧ (b ∧ c) ≠ (a ∧ b) ∧ c" [a, b, c]
  show (BMSLVNonCommutative a b) = withArgs "a ∧ b ٍ≠ b ∧ a" [a, b]
  show (BMSLVNonIdempotent a) = withArgs "a ∧ a ≠ a" [a]
  show (BMSLVNonUnital a) = withArgs "a ∧ top ≠ a" [a]
  show (BMSLVMeetOrderViolation a b) = withArgs "a ∧ b > a" [a, b]

withArgs :: Show a => String -> [a] -> String
withArgs s bs = s ++ "\n\t" ++ foldr (<>) mempty (intersperse "\n\t" (map show bs))

-- |
-- Verifies bounded meet semilattice laws.
prop_BoundedMeetSemiLattice
  :: (BoundedMeetSemiLattice a, Ord a, Eq a, Show a)
  => a -> a -> a -> CounterExample (BoundedMeetSemiLatticeLawViolation a)
prop_BoundedMeetSemiLattice a b c =
  annotate (BMSLVNonAssociative a b c) ((a /\ (b /\ c)) === ((a /\ b) /\ c))
  /\ annotate (BMSLVNonCommutative a b) ((a /\ b) === (b /\ a))
  /\ annotate (BMSLVNonIdempotent a) ((a /\ a) === a)
  /\ annotate (BMSLVNonUnital a) ((top /\ a) === a)
  /\ annotate (BMSLVMeetOrderViolation a b) ((Meet (a /\ b) `leq` Meet a) === True)

data BoundedJoinSemiLatticeLawViolation a
  = BJSLVNonAssociative a a a
  | BJSLVNonCommutative a a
  | BJSLVNonIdempotent a
  | BJSLVNonUnital a
  | BJSLVJoinOrderViolation a a
  deriving (Eq, Ord)

instance Show a => Show (BoundedJoinSemiLatticeLawViolation a) where
  show (BJSLVNonAssociative a b c) = withArgs "a ∨ (b ∨ c) ≠ (a ∨ b) ∨ c" [a, b, c]
  show (BJSLVNonCommutative a b) = withArgs "a ∨ b ٍ≠ b ∨ a" [a, b]
  show (BJSLVNonIdempotent a) = withArgs "a ∨ a ≠ a" [a]
  show (BJSLVNonUnital a) = withArgs "a ∨ top ≠ a" [a]
  show (BJSLVJoinOrderViolation a b) = withArgs "a ∨ b > a" [a, b]

-- |
-- Verifies bounded join semilattice laws.
prop_BoundedJoinSemiLattice
  :: (BoundedJoinSemiLattice a, Ord a, Eq a, Show a)
  => a -> a -> a -> CounterExample (BoundedJoinSemiLatticeLawViolation a)
prop_BoundedJoinSemiLattice a b c =
     annotate (BJSLVNonAssociative a b c)
        ((a \/ (b \/ c)) === ((a \/ b) \/ c))
  /\ annotate (BJSLVNonCommutative a b)
        ((a \/ b) === (b \/ a))
  /\ annotate (BJSLVNonIdempotent a)
        ((a \/ a) === a)
  /\ annotate (BJSLVNonUnital a)
        ((bottom \/ a) === a)
  /\ fromBool (BJSLVJoinOrderViolation a b)
        (Join a `leq` Join (a \/ b))

data DistributiveLatticeLawViolation a
  = DLLVJoinOverMeetViolation a a a
  | DLLVMeetOverJoinViolation a a a
  deriving (Eq, Ord)

instance Show a => Show (DistributiveLatticeLawViolation a) where
  show (DLLVJoinOverMeetViolation a b c) = withArgs "a ∧ (b ∨ c) ≠ a ∧ b ∨ a ∧ c" [a, b, c]
  show (DLLVMeetOverJoinViolation a b c) = withArgs "a ∨ (b ∧ c) ≠ (a ∨ b) ∧ (a ∨ c)" [a, b, c]

-- |
-- Distributivity laws for a lattice.
prop_DistributiveLattice
  :: (Lattice a, Ord a, Eq a, Show a)
  => a -> a -> a -> CounterExample (DistributiveLatticeLawViolation a)
prop_DistributiveLattice a b c =
     annotate (DLLVJoinOverMeetViolation a b c)
      ((a /\ (b \/ c)) === ((a /\ b) \/ (a /\ c)))
  /\ annotate (DLLVMeetOverJoinViolation a b c)
      ((a \/ (b /\ c)) === ((a \/ b) /\ (a \/ c)))

data HeytingAlgebraLawViolation a
  = HAVImplication1 a a a
  | HAVImplication2 a a a
  | HAVNot a a
  | HAVNotAndMeet a a
  | HAVNotAndJoin a a
  | HAVImplicationAndOrd a a
  | HAVDistributiveLatticeLawViolation (DistributiveLatticeLawViolation a)
  | HAVBoundedJoinSemilatticeLawViolation (BoundedJoinSemiLatticeLawViolation a)
  | HAVBoundedMeetSemilatticeLawViolation (BoundedMeetSemiLatticeLawViolation a)
  deriving (Eq, Ord)

instance Show a => Show (HeytingAlgebraLawViolation a) where
  show (HAVImplication1 x a b)=
    withArgs "x ≤ (a ⇒ b) then x ∧ a ≤ b" [x, a, b]

  show (HAVImplication2 x a b) =
      withArgs "x ∧ a ≤ b then x ≤ (a ⇒ b)" [x, a, b]

  show (HAVNot a b) =
    withArgs "a ≤ b ⇏ not a" [a, b]

  show (HAVNotAndMeet a b) =
    withArgs "not (a ∧ b) ≠ not a ∨ not b" [a, b]

  show (HAVNotAndJoin a b) =
    withArgs "not (a ∨ b) ≠ not a ∧ not b" [a, b]

  show (HAVImplicationAndOrd a b) =
    withArgs "(a ⇒ b) ∧ a ≰ b" [a, b]

  show (HAVDistributiveLatticeLawViolation e)    = show e
  show (HAVBoundedJoinSemilatticeLawViolation e) = show e
  show (HAVBoundedMeetSemilatticeLawViolation e) = show e

-- |
-- Verifies the Heyting algebra law for @==>@:
-- for all @a@: @_ /\ a@ is left adjoint to  @a ==>@
-- and some other properties that are a consequence of that.
prop_implies :: (HeytingAlgebra a, Ord a, Eq a, Show a)
             => a -> a -> a -> CounterExample (HeytingAlgebraLawViolation a)
prop_implies x a b =
    fromBool
      (HAVImplication1 x a b)
      (Meet x `leq` Meet (a ==> b) ==> (Meet (x /\ a) `leq` Meet b))
  /\ fromBool
      (HAVImplication2 x a b)
      (Meet (x /\ a) `leq` Meet b ==> (Meet x `leq` Meet (a ==> b)))
  /\ fromBool
      (HAVNot a b)
      (Meet a `leq` Meet b ==> (Meet (not b) `leq` Meet (not a)))
  /\ annotate
      (HAVNotAndMeet a b)
      (not (a /\ b) === (not a \/ not b))
  /\ annotate
      (HAVNotAndJoin a b)
      (not (a \/ b) === (not a /\ not b))
  /\ fromBool
      (HAVImplicationAndOrd a a)
      (Meet ((a ==> b) /\ a) `leq` Meet b)

-- |
-- Useful for testing valid instances of @'HeytingAlgebra'@ type class. It
-- validates:
--
-- * bounded lattice laws
-- * @'prop_implies'@
prop_HeytingAlgebra
  :: (HeytingAlgebra a, Ord a, Eq a, Show a)
  => a -> a -> a -> CounterExample (HeytingAlgebraLawViolation a)
prop_HeytingAlgebra a b c =

     fmapCounterExample HAVBoundedJoinSemilatticeLawViolation
        (prop_BoundedJoinSemiLattice a b c)

  /\ fmapCounterExample HAVBoundedMeetSemilatticeLawViolation
        (prop_BoundedMeetSemiLattice a b c)

  /\ fmapCounterExample HAVDistributiveLatticeLawViolation
        (prop_DistributiveLattice a b c)

  /\ prop_implies a b c
