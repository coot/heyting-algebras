# Heyting Algebras
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Travis Build Status](https://travis-ci.org/coot/heyting-algebra.svg?branch=master)](https://travis-ci.org/coot/heyting-algebra)

This package contains type classes and instances for many Heyting algebras
which are in the Haskell eco-system.  It is build on top of
[lattices](https://hackage.haskell.org/package/lattices) and
[free-algebras](https://hackage.haskell.org/package/free-algebras) (to provide
combinators for free Heyting algebras).  The package also contains Boolean algebras.

A very good introduction to Heyting algebras can be found at
[ncatlab](https://ncatlab.org/nlab/show/Heyting%2Balgebra).  Heyting algebras
are the crux of [intuitionistic
logic](https://en.wikipedia.org/wiki/Intuitionistic_logic), which drops the
axiom of exluded middle.  From categorical point of view, Heyting algebras are
posets (categories with at most one arrow between any objects), which are also
Cartesian closed (and finitely (co-)complete).  This is exactly the same kind
of category that simply typed lambda calculus come from: hence one more
incentive to learn about them!

The most important addition is the implication `(==>) :: HeytingAlgebra a =>
a -> a -> a`; since every Boolean algebra is a Heyting algebra via `a ==>
b = not a \/ b` (using the lattice notation for `or`).  It is very handy in
expression conditional logic.
