module Main where

import Test.QuickCheck

import Data.HeytingAlgebra


main :: IO ()
main = do
  quickCheck (prop_HeytingAlgebra @Bool)
  quickCheck (prop_HeytingAlgebra @(Maybe Bool))
  quickCheck (prop_HeytingAlgebra @(Maybe (Maybe Bool)))
  quickCheck (prop_HeytingAlgebra @(Maybe (Maybe (Maybe Bool))))

  -- quickCheck (prop_HeytingAlgebra @(Either String Bool))

