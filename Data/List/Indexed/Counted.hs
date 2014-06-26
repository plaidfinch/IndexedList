{- |
Module      :  Data.List.Indexed.Counted
Description :  A library providing length-indexed and element-indexed lists which sit somewhere between homogeneous and fully heterogeneous lists.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module implements homogeneous counted lists, which are type-indexed by length.
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

module Data.List.Indexed.Counted where

import Data.Numeric.Witness.Peano

import Control.Applicative
import Data.List hiding ( replicate , zip )
import Prelude   hiding ( replicate , zip )

infixr 5 :::

-- | Counted lists are indexed by length in the type.
data CountedList n a where
   (:::) :: a -> CountedList n a -> CountedList (Succ n) a
   CountedNil :: CountedList Zero a

instance (Show x) => Show (CountedList n x) where
   showsPrec p xs = showParen (p > 10) $
      (showString $ ( intercalate " ::: "
                    $ map show
                    $ unCount xs ) ++ " ::: CountedNil")

-- | The 'count' of a list is the natural number corresponding to its length.
count :: CountedList n a -> Natural n
count CountedNil = Zero
count (x ::: xs) = Succ (count xs)

-- | Convert a counted list to a regular Haskell list.
unCount :: CountedList n a -> [a]
unCount CountedNil       = []
unCount (x ::: xs) = x : unCount xs

-- | Replicate some element a certain number of times.
replicate :: Natural n -> x -> CountedList n x
replicate Zero     _ = CountedNil
replicate (Succ n) x = x ::: replicate n x

-- | Appending two counted lists yields one of length equal to the sum of the lengths of the two initial lists.
append :: CountedList n a -> CountedList m a -> CountedList (m + n) a
append CountedNil       ys = ys
append (x ::: xs) ys = x ::: append xs ys

-- | Take the nth element of a list. We statically prevent taking the nth element of a list of length less than n.
nth :: (n < m) => Natural n -> CountedList m a -> a
nth Zero     (a ::: _)  = a
nth (Succ n) (_ ::: as) = nth n as
nth _ _ = error "nth: the impossible occurred" -- like in minus, GHC can't prove this is unreachable

-- | Pad the length of a list to a given length. If the number specified is less than the length of the list given,
--   it won't pass the type-checker.
padTo :: (m <= n) => Natural n -> x -> CountedList m x -> CountedList ((n - m) + m) x
padTo n x list =
   list `append` replicate (n `minus` count list) x

-- | Zip two equal-length lists together.
zip :: CountedList n a -> CountedList n b -> CountedList n (a,b)
zip (a ::: as) (b ::: bs) = (a,b) ::: zip as bs
zip CountedNil _ = CountedNil
zip _ CountedNil = CountedNil

instance Functor (CountedList n) where
   fmap f CountedNil = CountedNil
   fmap f (a ::: as) = f a ::: fmap f as

instance (ReifyNatural n) => Applicative (CountedList n) where
   pure x    = replicate (reifyNatural :: Natural n) x
   fs <*> xs = uncurry id <$> zip fs xs
