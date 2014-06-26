{- |
Module      :  Data.List.Indexed.Conic
Description :  A library providing length-indexed and element-indexed lists which sit somewhere between homogeneous and fully heterogeneous lists.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module implements conic lists, which are lists where every element is of type @(f a)@ for /some/ @a@, but the @a@ 
index may vary. This sits between homogeneous and fully heterogeneous lists in terms of expressivity and also the ease
to manipulate.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-} 

module Data.List.Indexed.Conic where

import Data.Numeric.Witness.Peano

infixr 5 :-:
data x :-: y
data Nil

-- | Conic lists are lists where each element is an (f a) for some a, but the a may be different for each element. Types of elements are kept track of in the type of the list.
data ConicList f ts where
   (:-:) :: f a -> ConicList f rest -> ConicList f (a :-: rest)
   ConicNil  :: ConicList f Nil

type family Replicate n x where
   Replicate Zero     x = Nil
   Replicate (Succ n) x = x :-: Replicate n x

type family Length ts where
   Length Nil        = Zero
   Length (x :-: xs) = Succ (Length xs)

type family Tack x xs where
   Tack a Nil        = a :-: Nil
   Tack a (x :-: xs) = x :-: Tack a xs

tack :: f t -> ConicList f ts -> ConicList f (Tack t ts)
tack a ConicNil   = a :-: ConicNil
tack a (x :-: xs) = x :-: tack a xs
