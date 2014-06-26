{- |
Module      :  Data.List.Indexed
Description :  A library providing length-indexed and element-indexed lists which sit somewhere between homogeneous and fully heterogeneous lists.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module re-exports the 'ConicList' and 'CountedList' types and functions to work with them.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}

module Data.List.Indexed
   ( heterogenize , homogenize
   , module Data.List.Indexed.Counted
   , module Data.List.Indexed.Conic
   ) where

import Data.List.Indexed.Counted
import Data.List.Indexed.Conic

-- We can convert between them using cones and co-cones

-- | Turn a 'CountedList' into a 'ConicList' by means of a function from some @a@ to an @(f t)@.
heterogenize :: (a -> f t) -> CountedList n a -> ConicList f (Replicate n t)
heterogenize _ CountedNil = ConicNil
heterogenize f (x ::: xs) = f x :-: heterogenize f xs

-- | Given a function to collapse any @(f t)@ into an @a@, turn a 'ConicList' into a 'CountedList'.
homogenize :: (forall t. f t -> a) -> ConicList f ts -> CountedList (Length ts) a
homogenize _ ConicNil   = CountedNil
homogenize f (x :-: xs) = f x ::: homogenize f xs
