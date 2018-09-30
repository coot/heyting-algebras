{-# LANGUAGE ScopedTypeVariables #-}
module Algebra.Heyting
  ( HeytingAlgebra (..)
  , lessOrEq
  , greaterOrEq
  , toBool
  , HBool
  , hBool

    -- * Properties
  , prop_HeytingAlgebra
  , prop_conj
  , prop_disj
  , prop_implies
  )
  where

import Prelude hiding (not, (||), (&&))
import qualified Prelude

import Test.QuickCheck

import Algebra.Boolean (BooleanAlgebra)
import qualified Algebra.Boolean as B


class HeytingAlgebra a where
  ff :: a
  tt :: a

  implies :: a -> a -> a

  (||) :: a -> a -> a
  (&&) :: a -> a -> a

not :: HeytingAlgebra a => a -> a
not a = a `implies` ff

infixr 3 &&
infixr 2 ||

instance HeytingAlgebra Bool where
  ff = False
  tt = True
  implies = B.implies
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)

instance HeytingAlgebra () where
  ff = ()
  tt = ()
  implies _ _ = ()
  _ && _  = ()
  _ || _  = ()

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
  ff _ = ff
  tt _ = tt
  implies f g = \a -> f a `implies` g a
  f && g  = \a -> f a && g a
  f || g  = \a -> f a || g a

instance (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (a, b) where
  ff = (ff, ff)
  tt = (tt, tt)
  implies (a0, b0) (a1, b1) = (a0 `implies` a1, b0 `implies` b1)
  (a0, b0) && (a1, b1) = (a0 && a1, b0 && b1)
  (a0, b0) || (a1, b1) = (a0 || a1, b0 || b1)

--     tt = Just tt
--        |
--     Just a         
--        |
--     Just ff
--        |
--     ff = Nothing
instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Maybe a) where
  ff = Nothing
  tt = Just tt

  implies (Just a)(Just b)  = Just (a `implies` b)
  implies Nothing  _        = Just tt
  implies _        Nothing  = Nothing

  Just a  && Just b  = Just (a && b)
  _       && Nothing = Nothing
  Nothing && _       = Nothing

  Just a  || Just b  = Just (a || b)
  Just a  || Nothing = Just a
  Nothing || Just a  = Just a
  Nothing || Nothing = Nothing

-- |
-- Every Heyting algebra contains a Boolean algebra. @'toBool'@ maps onto it;
-- moreover its a monad (Heyting algebra is a category as every poset is) which
-- preserves finite infima.
toBool :: HeytingAlgebra a => a -> a
toBool = not . not

-- |
-- @'HBool'@ is the left adjoint functor from the category of Heyting algebras
-- to the category of Boolean algebra; its right adjoint is the inclusion
-- (every Boolean algebra is a Heyting algebra).
newtype HBool a = HBool a

hBool :: HeytingAlgebra a => a -> HBool a
hBool = HBool . toBool

instance HeytingAlgebra a => BooleanAlgebra (HBool a) where
  ff = HBool ff
  tt = HBool tt

  HBool a && HBool b = HBool (a && b)
  HBool a || HBool b = HBool (a || b)

  not (HBool a) = HBool (not a)

lessOrEq :: (HeytingAlgebra a, Eq a) => a -> a -> Bool
lessOrEq a b = (a && b) == a

greaterOrEq :: (HeytingAlgebra a, Eq a) => a -> a -> Bool
greaterOrEq a b = (a || b) == a

--
-- Properties
--

prop_conj :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> a -> Property
prop_conj a b c =
       counterexample "conj associativity" ((a && (b && c)) === ((a && b) && c))
  .&&. counterexample "conj commutativity" ((a && b) === (b && a))
  .&&. counterexample "conj idempotent" ((a && a) === a)
  .&&. counterexample "conj identity" ((tt && a) === a)
  .&&. counterexample "conj absorption" ((ff && a) == ff)
  .&&. counterexample "conj order" ((a && b) `lessOrEq` a)

prop_disj :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> a -> Property
prop_disj a b c =
       counterexample "disj associativity" ((a || (b || c)) === ((a || b) || c))
  .&&. counterexample "disj commutativity" ((a || b) === (b || a))
  .&&. counterexample "disj idempotent" ((a || a) === a)
  .&&. counterexample "disj identity" ((ff || a) === a)
  .&&. counterexample "disj absoroption" ((tt || a) === tt)
  .&&. counterexample "disj order" ((a || b) `greaterOrEq` a)

-- For all `a`: `_ && a` is left adjoint to  `implies a`
prop_implies :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> Property
prop_implies a b =
       counterexample
        (show a ++ " ⇒ " ++ show b ++ " && " ++ show a ++ " NOT ≤ " ++ show b)
        ((a `implies` b && a) `lessOrEq` b)
  .&&. counterexample
        (show b ++ " NOT ≤ " ++ show a ++ " ⇒ " ++ show a ++ " && " ++ show b)
        (b `lessOrEq` (a `implies` a && b))

-- |
-- Usefull for testing valid instances of `HeytingAlgebra` type class.
prop_HeytingAlgebra :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> a -> Property
prop_HeytingAlgebra a b c =
       prop_disj a b c
  .&&. prop_conj a b c
  .&&. prop_implies a b
