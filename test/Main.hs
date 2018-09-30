module Main where

import Test.QuickCheck

import Algebra.Heyting

main :: IO ()
main = do
  quickCheck (prop_HeytingAlgebra @Bool)
  quickCheck (prop_HeytingAlgebra @(Maybe Bool))
  quickCheck (prop_HeytingAlgebra @(Maybe (Maybe Bool)))
  quickCheck (prop_HeytingAlgebra @(Maybe (Maybe (Maybe Bool))))
