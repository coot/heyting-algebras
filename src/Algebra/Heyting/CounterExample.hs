{-# LANGUAGE BangPatterns #-}
module Algebra.Heyting.CounterExample where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Printf (printf)

import           Algebra.Lattice (top)
import           Algebra.Lattice.Lifted (Lifted)
import qualified Algebra.Lattice.Lifted as Lifted
import           Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as Levitated
import           Algebra.Lattice.Op (Op (..))


-- | A counter example type is a Heyting algebra; it useful for tests and
-- properties.  It records all failures.  The truth value is represented by an
-- empty set.  Since @'CounterExample' e@ is a Heyting algebra, it is useful in
-- expressing properties that require assumptions.
--
type CounterExample a = Lifted (Op (Set a))

fmapCounterExample
  :: (Ord a, Ord b)
  => (a -> b)
  -> CounterExample a
  -> CounterExample b
fmapCounterExample = fmap . fmap . Set.map

-- | A bijection from @e@ to atoms of the @'CounterExample' e@ lattice.
--
counterExample :: e -> CounterExample e
counterExample e = Lifted.Lift (Op (Set.singleton e))

-- | A lattice homorphism from @'Bool'@ to @'CounterExample'@, which lifts
-- @'False'@ to an atom of @'CounterExample'@ (uniquelly determined by @e@) and which preserves the top element.
--
fromBool :: Ord e => e -> Bool -> CounterExample e
fromBool e False = counterExample e
fromBool _ True  = top

-- |
-- A homomorphism of /Heyting algebras/.
--
toBool :: CounterExample e -> Bool
toBool (Lifted.Lift (Op s)) | Set.null s
                            = True
toBool _                    = False

-- | Note that this map is not a lattice homomorphism (it does not preserve
-- meet nor join).  It is also not a posset map in general.  Nevertheless, it preserves
-- @top@ and @bottom@.
--
foldMapCounterExample
  :: (Ord e, Monoid m)
  => (e -> m)
  -> CounterExample e
  -> Levitated m
foldMapCounterExample f (Lifted.Lift (Op s)) | Set.null s = Levitated.Top
                                             | otherwise  = Levitated.Levitate (foldMap f s)
foldMapCounterExample _ Lifted.Bottom        = Levitated.Bottom

-- |
-- Map a CounterExample to @'Levitated' String@.  Each set of counter example
-- is concatenated into a single comma separated string.  This is useful for
-- printing counter examples in tests.  See @'fromCounterExample''@.
--
fromCounterExample :: Show a => CounterExample a -> Levitated String 
fromCounterExample Lifted.Bottom        = Levitated.Bottom
fromCounterExample (Lifted.Lift (Op s)) | Set.null s
                                        = Levitated.Top
                                        | otherwise
                                        = Levitated.Levitate (Set.foldl' go "" s)
  where
    go "" b = show b
    go !a b = printf "%s, %s" a (show b)

-- | Map `CounterExample` to a Maybe, representing @'Levitated.Top'@ as
-- @'Nothing'@ and mapping both @'Levitated.Levitate'@ and @'Levitate.Bottom'@ to
-- a @Just String@.
--
fromCounterExample' :: Show a => CounterExample a -> Maybe String
fromCounterExample' ce = case fromCounterExample ce of
  Levitated.Top        -> Nothing
  Levitated.Levitate s -> Just s
  Levitated.Bottom     -> Just ""

-- | Add a counter example.  This is simply lifts the bottom to an atom given
-- by @e@, otherwise it preseves the @'CounterExample' e@.
--
-- Note that take join of @\e es = counterExample e \\/ es@ will return @top@ if
-- @e@ if e is not in the set @es@; thus this map is not defined with using @\\/@.
--
annotate :: Ord e => e -> CounterExample e -> CounterExample e
annotate e Lifted.Bottom = counterExample e
annotate _ es     = es

(===) :: (Ord e, Eq a) => a -> a -> CounterExample e
a === b = if a == b then Lifted.Lift (Op Set.empty) else Lifted.Bottom

infixr 4 ===
