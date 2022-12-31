{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Data.MarkerSeqs
-- Copyright   :  (c) Oleksandr Zhabenko 2022
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
--

module Data.MarkerSeqs where

import GHC.Num
import GHC.Real
import GHC.Base
import Data.List
import GHC.Show
import Data.Bits
import Numeric (showIntAtBase,showInt)
import Data.Foldable hiding (length)
import GHC.Int
--import qualified Data.Heap as H

showBin :: Int -> [Char]
showBin x = '0':'b':showIntAtBase 2 (head . flip showInt "") x ""
{-# INLINE showBin #-}

unionCount :: (Bits a) => a -> a -> Int
unionCount x = popCount . (.&.) x
{-# INLINE unionCount #-}

countWeightsQs :: (Foldable t) => [t a -> Int] -> [t a] -> [[Int]]
countWeightsQs fs xs = map (flip map xs) fs

data Sort2 a = S2 { 
 id :: Int8,
 val :: a
}

instance Eq a => Eq (Sort2 a) where
  S2 _ x == S2 _ y = x == y

instance Ord a => Ord (Sort2 a) where
  compare (S2 _ x) (S2 _ y) = compare y x
  S2 _ x > S2 _ y = x < y
  S2 _ x < S2 _ y = x > y
  S2 _ x >= S2 _ y = x <= y
  S2 _ x <= S2 _ y = x >= y

instance Functor Sort2 where
  fmap f (S2 k x) = S2 k . f $ x

instance Show a => Show (Sort2 a) where
  show (S2 k x) = show k ++ '~':show x

data ASort3 a = As3 { 
 id3 :: Int8,
 orD :: Int8,
 val3 :: a
}

instance Eq a => Eq (ASort3 a) where
  As3 _ _ x == As3 _ _ y = x == y

instance Show a => Show (ASort3 a) where
  show (As3 n k x) = show n ++ '&':show k ++ '~':show x

splitF :: Int -> [a] -> [[a]]
splitF n ys =
   let q = length ys `quot` n
       rs = take (q * n) ys in f' n rs
        where f' n ks@(_:_) = let (ts,ws) = splitAt n ks in ts : f' n ws
              f' _ _ = []

getIndices2 
  :: Ord a => Int8 
  -> [Int8] 
  -> [a] 
  -> [[[Int8]]]
getIndices2 groupLength ks xs = filter (not. null) . map (filter (not . null). map (idList ks) . filter (not . null) . g [groupLength-1,groupLength-2..] . sort . 
   zipWith S2 [groupLength-1,groupLength-2..]) . splitF (fromIntegral groupLength) $ xs
     where g (q:qs) xs@(x:ys) = let (js,rs) = span (== x) ys in map (\(S2 k y) -> As3 k q y) (x:js) : g qs rs
           g _ _ = []

idList :: Eq a => [Int8] -> [ASort3 a] -> [Int8]
idList orDs ys = map (\(As3 k _ _) -> k) . filter (\(As3 _ n _) -> n `elem` orDs) $ ys
