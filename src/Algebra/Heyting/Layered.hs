module Algebra.Heyting.Layered
  ( Layered (..)
  , layer
  ) where

import           Prelude

import           Algebra.Lattice ( BoundedJoinSemiLattice (..)
                                 , BoundedMeetSemiLattice (..)
                                 , Lattice (..)
                                 )

import           Algebra.Heyting ( Heyting (..) )

-- |
-- Layer one Heyting algebra on top of the other.  Note: this is not
-- a categorical sum.
data Layered a b
  = Lower a
  | Upper b
  deriving (Show, Eq, Ord)

instance ( Lattice a
         , Lattice b
         ) => Lattice (Layered a b) where
  l@Lower{} /\ Upper _   = l
  Upper _   /\ l@Lower{} = l
  Lower l   /\ Lower l'  = Lower $ l /\ l'
  Upper u   /\ Upper u'  = Upper $ u /\ u'

  Lower _   \/ u@Upper{} = u
  u@Upper{} \/ Lower _   = u
  Lower l   \/ Lower l'  = Lower $ l \/ l'
  Upper u   \/ Upper u'  = Upper $ u \/ u'

instance (BoundedJoinSemiLattice a, Lattice b) => BoundedJoinSemiLattice (Layered a b) where
  bottom = Lower bottom

instance (Lattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (Layered a b) where
  top = Upper top

instance ( Heyting a
         , Heyting b
         , Eq a
         ) => Heyting (Layered a b) where
  Lower _   ==> Upper _      = Upper top
  Upper _   ==> l@Lower{}    = l
  Upper u   ==> Upper u'     = Upper $ u ==> u'
  Lower l   ==> Lower l'     = case l ==> l' of
    ll' | ll' == top -> Upper top
        | otherwise  -> Lower ll'

layer :: (a -> c) -> (b -> c) -> Layered a b -> c
layer f _ (Lower a) = f a
layer _ g (Upper b) = g b
