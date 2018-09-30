{-# LANGUAGE ScopedTypeVariables #-}
module Data.HeytingAlgebra
  ( HeytingAlgebra (..)
  , less
  , greater

  -- * Properties
  , prop_HeytingAlgebra
  )
  where

import Prelude hiding (not, (||), (&&))
import qualified Prelude

import Test.QuickCheck

class HeytingAlgebra a where
  ff :: a
  tt :: a

  implies :: a -> a -> a
  implies a b = not a || b

  (||) :: a -> a -> a
  (&&) :: a -> a -> a

  not :: a -> a
  not a = a `implies` ff

infixr 3 &&
infixr 2 ||

instance HeytingAlgebra Bool where
  ff = False
  tt = True
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  not = Prelude.not

less :: (HeytingAlgebra a, Eq a) => a -> a -> Bool
less a b = (a && b) == a

greater :: (HeytingAlgebra a, Eq a) => a -> a -> Bool
greater a b = (a || b) == a

instance HeytingAlgebra () where
  ff = ()
  tt = ()
  implies _ _ = ()
  _ && _  = ()
  _ || _  = ()
  not _   = ()

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
  ff _ = ff
  tt _ = tt
  implies f g a = f a `implies` g a
  f && g  = \a -> f a && g a
  f || g  = \a -> f a || g a
  not f a = not (f a)

instance (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (a, b) where
  ff = (ff, ff)
  tt = (tt, tt)
  implies (a0, b0) (a1, b1) = (a0 `implies` a1, b0 `implies` b1)
  (a0, b0) && (a1, b1) = (a0 && a1, b0 && b1)
  (a0, b0) || (a1, b1) = (a0 || a1, b0 || b1)
  not (a0, b0) = (not a0, not b0)

--     Just tt
--        |
--     Just a         
--        |
--     Just ff
--        |
--     Nothing
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
  .&&. counterexample "conj order" ((a && b) `less` a)

prop_disj :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> a -> Property
prop_disj a b c =
       counterexample "disj associativity" ((a || (b || c)) === ((a || b) || c))
  .&&. counterexample "disj commutativity" ((a || b) === (b || a))
  .&&. counterexample "disj idempotent" ((a || a) === a)
  .&&. counterexample "disj identity" ((ff || a) === a)
  .&&. counterexample "disj absoroption" ((tt || a) === tt)
  .&&. counterexample "disj order" ((a || b) `greater` a)
  where

-- |
-- For all `a`: `_ && a` is left adjoint to  `implies a`
prop_implies :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> Property
prop_implies a b =
       counterexample
        (show a ++ " ⇒ " ++ show b ++ " && " ++ show a ++ " NOT ≤ " ++ show b)
        ((a `implies` b && a) `less` b)
  .&&. counterexample
        (show b ++ " NOT ≤ " ++ show a ++ " ⇒ " ++ show a ++ " && " ++ show b)
        (b `less` (a `implies` a && b))

-- |
-- Usefull for testing valid instances of `HeytingAlgebra` type class.
prop_HeytingAlgebra :: forall a. (HeytingAlgebra a, Eq a, Show a) => a -> a -> a -> Property
prop_HeytingAlgebra a b c =
       prop_disj a b c
  .&&. prop_conj a b c
  .&&. prop_implies a b
