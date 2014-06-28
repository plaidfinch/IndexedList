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

data IndexedMaybe a i where
   IsJust    :: a -> IndexedMaybe a IsJust
   IsNothing :: IndexedMaybe a IsNothing

y :: ConicList (IndexedMaybe Int) (IsJust :-: IsNothing :-: IsJust :-: Nil)
y = IsJust 1 :-: IsNothing :-: IsJust 2 :-: ConicNil
```
