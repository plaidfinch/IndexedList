IndexedList
===========

This library implements *counted lists* linked to type-level naturals indexing length, compatible with the Peano natural numbers found in `Data.Numeric.Witness.Peano`, as well as so-called *conic lists*, which are linked to a type index listing the type indices of partially-heterogeneous values contained within.

Example of using a counted list:

```Haskell
x :: CountedList (Succ (Succ (Succ Zero))) Int
x = 1 ::: 2 ::: 3 ::: CountedNil
```

Example of using a conic list:

```Haskell
{-# LANGUAGE GADTs #-}

data IsJust
data IsNothing

data IndexedMaybe i a where
   IsJust    :: a -> IndexedMaybe IsJust a
   IsNothing :: IndexedMaybe IsNothing a

y :: ConicList (IsJust :-: IsNothing :-: IsJust :-: Nil) Int
y = IsJust 1 :-: IsNothing :-: IsJust 2 :-: ConicNil
```
