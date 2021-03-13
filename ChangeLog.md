# Revision history for heyting-algebra

## 0.1.0.0

* Swapped Boolean (now a type class) and BooleanAlgebra (now a data type)
* Reexport Algebra.Heyting and Algebra.Heyting.Free modules from lattices

## 0.0.2.0

* Added Algebra.Heyting.CounterExample
* Added Algebra.Heyting.Free.atom
* Added `BoolRing` a Boolean ring
* Check distributivity laws
* newtype `Ordered` adds Heyting algebra instance for every type satisfying the
  `Ord` constraint.
* (<=>) operator added
* Library does not depens on QuickCheck anymore

## 0.0.1.1 -- 2018.10.5

* First version. Released on an unsuspecting world.
