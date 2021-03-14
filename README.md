# Heyting Algebras
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Travis Build Status](https://travis-ci.org/coot/heyting-algebras.svg?branch=master)](https://travis-ci.org/coot/heyting-algebras)

This package now extends
[lattices](https://hackage.haskell.org/package/lattices) by providing more
Heyting algebras.  The package also defines a type class for Boolean algebras
and comes with many useful instances.

A note about notation: this package is based on
[lattices](https://hackage.haskell.org/package/lattices), and both are using
notation and names common in lattice theory and logic.  Where `&&` becomes `∧`
and is called `meet` and `||` is denoted by `∨` and is usually called
`join`.  The `lattice` package provides `\/` and `/\` operators as well as type
classes for various flavors of posets and lattices.

A very good introduction to Heyting algebras can be found at
[ncatlab](https://ncatlab.org/nlab/show/Heyting%2Balgebra).  Heyting algebras
are the crux of [intuitionistic
logic](https://en.wikipedia.org/wiki/Intuitionistic_logic), which drops the
axiom of excluded middle.  From categorical point of view, Heyting algebras are
posets (categories with at most one arrow between any objects), which are also
Cartesian closed (and finitely (co-)complete).  Note that this makes any
Heyting algebra a simply typed lambda calculus; hence one more incentive to
learn about them.  For example currying holds in every Heyting algebra:
`a => (b ⇒ c)` is equal to `(a ∧ b) ⇒ c`

The most important operation is implication `(==>) :: HeytingAlgebra a => a ->
a -> a` (which we might also write as ⇒ in documentation).  Every Boolean
algebra is a Heyting algebra via `a ==> b = not a \/ b` (using the lattice
notation for `or`).  It is very handy in expression conditional logic.

Some examples of Heyting algebras:
* `Bool` is a Boolean algebra
* `(Ord a, Bounded a) => a`; the implication is defined as: if `a ≤ b` then `a
  ⇒ b = maxBound`, otherwise `a ⇒ b = b`; e.g. integers with both `±∞` (it can
  be represented by `Levitated Int`.  Note that it is not a Boolean algebra.
* The power set is a Boolean algebra, in Haskell it can be represented by `Set
  a` (one might need to require `a` to be finite though, otherwise `not (not
  empty)` might be `undefined` rather than `empty`).  It is a well known fact
  that every Boolean algebra is isomorphic to a power set.
* ```haskell
    type CounterExample a = Lifted (Op (Set a))
  ```
  is a Heyting algebra; it is useful for gathering counter examples in
  a similar way that `Property` from `QuickCheck` library does (put pure).
  This library provides some useful functions for this type, see the
  `Algebra.Heyting.Properties` and tests for example usage.
* More generally every type `(Ord k, Finite k, HeytingAlgebra v) => Map k a` is
  a Heyting algebra (though in general not a Boolean one).
