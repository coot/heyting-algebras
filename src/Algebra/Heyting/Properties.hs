{-# LANGUAGE BangPatterns #-}
-- |
-- Properties of Heyting algebras; useful for testing lawfulness of instances.
--
module Algebra.Heyting.Properties where

import           Prelude hiding (not)

import           Data.List (intersperse)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Printf (printf)
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
import           Algebra.Lattice.Dropped (Dropped (..))
import           Algebra.Lattice.Op (Op (..))
import           Algebra.Heyting

-- | A counter example type is a lattice (with an upper bound), useful for
-- tests and properties.  It has the advantage over `Test.QuickCheck.Property`
-- that it records all failures.
--
type CounterExample = Dropped (Op (Set String))

counterExample :: String -> Bool -> CounterExample
counterExample _ True  = Top
counterExample e False = Drop (Op (Set.singleton e))

fromCounterExample :: CounterExample -> Maybe String 
fromCounterExample Top = Nothing
fromCounterExample (Drop (Op s)) = Just (Set.foldl' go "" s)
  where
    go "" b = b
    go !a b = printf "%s, %s" a b

annotate :: String -> CounterExample -> CounterExample
annotate _ Top = Top
annotate e (Drop (Op s)) = Drop (Op (Set.map (e <>) s))

(===) :: (Eq a, Show a) => a -> a -> CounterExample
a === b = counterExample (printf "%s /= %s" (show a) (show b)) (a == b)

infixr 4 ===

withBlinds :: Show a => String -> [a] -> String
withBlinds s bs = s ++ "\n\t" ++ foldr (<>) mempty (intersperse "\n\t" (map show bs))

-- |
-- Verifies bounded meet semilattice laws.
prop_BoundedMeetSemiLattice :: (BoundedMeetSemiLattice a, Eq a, Show a)
                            => a -> a -> a -> CounterExample
prop_BoundedMeetSemiLattice a b c =
  annotate (withBlinds "meet associativity" [a, b, c]) ((a /\ (b /\ c)) === ((a /\ b) /\ c))
  /\ annotate (withBlinds "meet commutativity" [a, b]) ((a /\ b) === (b /\ a))
  /\ annotate (withBlinds "meet idempotent" [a]) ((a /\ a) === a)
  /\ annotate (withBlinds "meet identity" [a]) ((top /\ a) === a)
  /\ annotate (withBlinds "meet order" [a, b]) ((Meet (a /\ b) `leq` Meet a) === True)

-- |
-- Verifies bounded join semilattice laws.
prop_BoundedJoinSemiLattice :: (BoundedJoinSemiLattice a, Eq a, Show a)
                            => a -> a -> a -> CounterExample
prop_BoundedJoinSemiLattice a b c =
     annotate (withBlinds "join associativity" [a, b, c])
        ((a \/ (b \/ c)) === ((a \/ b) \/ c))
  /\ annotate (withBlinds "join commutativity" [a, b])
        ((a \/ b) === (b \/ a))
  /\ annotate (withBlinds "join idempotent" [a])
        ((a \/ a) === a)
  /\ annotate (withBlinds "join identity" [a])
        ((bottom \/ a) === a)
  /\ counterExample (withBlinds "join order" [a, b])
        (Join a `leq` Join (a \/ b))

-- |
-- Distributivity laws for a lattice.
prop_DistributiveLattice :: (Lattice a, Eq a, Show a)
                         => a -> a -> a -> CounterExample
prop_DistributiveLattice a b c =
     annotate "a ∧ (b ∨ c) ≠ a ∧ b ∨ a ∧ c"
      ((a /\ (b \/ c)) === ((a /\ b) \/ (a /\ c)))
  /\ annotate "a ∨ (b ∧ c) ≠ (a ∨ b) ∧ (a ∨ c)"
      ((a \/ (b /\ c)) === ((a \/ b) /\ (a \/ c)))

-- |
-- Verifies the Heyting algebra law for @==>@:
-- for all @a@: @_ /\ a@ is left adjoint to  @a ==>@
-- and some other properties that are a consequence of that.
prop_implies :: (HeytingAlgebra a, Eq a, Show a)
             => a -> a -> a -> CounterExample
prop_implies x a b =
    counterExample
      (withBlinds "Failed: x ≤ (a ⇒ b) then x ∧ a ≤ b" [x, a, b])
      (Meet x `leq` Meet (a ==> b) ==> (Meet (x /\ a) `leq` Meet b))
  /\ counterExample
      (withBlinds "Failed: x ∧ a ≤ b then x ≤ (a ⇒ b)" [x, a, b])
      (Meet (x /\ a) `leq` Meet b ==> (Meet x `leq` Meet (a ==> b)))
  /\ counterExample
      (withBlinds "Failed: a ≤ b ⇏ not a" [a, b])
      (Meet a `leq` Meet b ==> (Meet (not b) `leq` Meet (not a)))
  /\ annotate
      (withBlinds "Failed: not (a ∧ b) ≠ not a ∨ not b" [a, b])
      (not (a /\ b) === (not a \/ not b))
  /\ annotate
      (withBlinds "Failed: not (a ∨ b) ≠ not a ∧ not b" [a, b])
      (not (a \/ b) === (not a /\ not b))
  /\ counterExample
      (withBlinds "Failed: (a ⇒ b) ∧ a ≰ b" [a, b])
      (Meet ((a ==> b) /\ a) `leq` Meet b)

-- |
-- Usefull for testing valid instances of @'HeytingAlgebra'@ type class. It
-- validates:
--
-- * bounded lattice laws
-- * @'prop_implies'@
prop_HeytingAlgebra :: (HeytingAlgebra a, Eq a, Show a)
                    => a -> a -> a -> CounterExample
prop_HeytingAlgebra a b c =
     prop_BoundedJoinSemiLattice a b c
  /\ prop_BoundedMeetSemiLattice a b c
  /\ prop_DistributiveLattice a b c
  /\ prop_implies a b c
