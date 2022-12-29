{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Data.FList
-- Copyright   :  (c) Oleksandr Zhabenko 2022
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Specially optimized lists that have instances of 'Eq' and 'Ord' classes just by the first element.
-- Are isomorphic to the non-empty lists. Can be converted to and from the non-empty lists.
-- Are intended to be the first data type to cover the data in for some algorithms using
-- 'Eq' and 'Ord' properties.

module Data.FList where

import GHC.Base
import GHC.List
import GHC.Show

infixl 5 %!

{-|
 -
 - -}
data FList a = F a | L a (FList a)

instance (Eq a) => Eq (FList a) where
  (==) (F x) (F y) = x == y
  (==) (L x _) (L t _) = x == t
  (==) _ _ = False

instance (Ord a) =>  Ord (FList a) where
  compare (F x) (F y) = compare x y
  compare (F x) (L t w) = compare x t
  compare (L x y) (F t) = compare x t
  compare (L x y) (L t w) = compare x t

instance (Show a) => Show (FList a) where
  show (F x) = '{':show x ++ "}"
  show (L x y) = '{':show x ++ "," ++ show2 y ++ "}"
    where show2 (F x) = show x
          show2 (L x y) = show x ++ "," ++ show2 y

(%!) :: a -> FList a -> FList a 
x %! y = L x y

toList :: FList a -> [a]
toList (F x) = [x]
toList (L x y) = x : toList y

-- | Is intended to be used just on the non-empty lists. Otherwise (just like 'head'), throw an error.
fromNonEmptyList :: [a] -> FList a
fromNonEmptyList (x:y:xs) = L x . fromNonEmptyList $ (y:xs)
fromNonEmptyList [x] = F x
fromNonEmptyList _ = error "Data.FList.fromNonEmptyList: you has given as the whole input the empty list."


