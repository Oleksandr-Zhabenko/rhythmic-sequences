{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MarkerSeqs
-- Copyright   :  (c) Oleksandr Zhabenko 2022-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Data and algorithmic basics to evaluate rhythmicity of the lists of 'Ord' instance data type.
-- Similar to @phonetic-languages-rhythmicity@ on Hackage.

module Data.MarkerSeqs where

import GHC.Num
import GHC.Real
import GHC.Base
import Data.List hiding (foldr)
import GHC.Show
import Data.Bits
import Numeric (showIntAtBase,showInt)
import Data.Foldable (Foldable)
import GHC.Int
import Data.Maybe (mapMaybe, catMaybes)
import Data.BasicF
import Text.Read

-- | The similar function is since @base-4.16.0.0@ in the 'Numeric' module. Is not used 
-- further, is provided here mostly for testing purposes.
showBin :: Int -> [Char]
showBin x = '0':'b':showIntAtBase 2 (head . flip showInt "") x ""
{-# INLINE showBin #-}

-- | Basic counting of the same bits in the 'Bits' arguments.
unionCount :: (Bits a) => a -> a -> Integer
unionCount x = toInteger . popCount . (.&.) x
{-# INLINE unionCount #-}

-- | Some idea function to evaluate the rhythmicity data. Is not used further in the package,
-- can be thought of as an alternative way of computation.
countWeightsQs :: (Foldable t) => [t a -> Int] -> [t a] -> [[Int]]
countWeightsQs fs xs = map (flip map xs) fs

-- | Data type used to provide somewhat \'array sorting with its indeces\'.
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

-- | Data type to contain the needed for hashing algorithm information about the sorted 
-- \'array sorting with its indeces\'.
data ASort3 a = As3 { 
 id3 :: Int8,
 orD :: Int8,
 val3 :: a
}

instance Eq a => Eq (ASort3 a) where
  As3 _ _ x == As3 _ _ y = x == y

instance Show a => Show (ASort3 a) where
  show (As3 n k x) = show n ++ '&':show k ++ '~':show x

-- | Split the list into lists of @n@ elements where @n@ is the first parameter.
splitF :: Int -> [a] -> [[a]]
splitF n ys =
   let q = length ys `quot` n
       rs = take (q * n) ys in f' n rs
        where f' n ks@(_:_) = let (ts,ws) = splitAt n ks in ts : f' n ws
              f' _ _ = []

-- | Function to get basic data for hash-based evaluation of the rhythmicity of the list data. Is
-- used internally in the 'countHashesG'.
-- Provided here mostly for testing purposes.
getHashes2 
  :: Ord a => Int8 -- ^ The period of the groups (the length of the lists into which the general sequence is splitted at first).
  -> [Int8] -- ^ The list must be sorted in the descending order, the elements must be greater than -1 and less than the first argument here and there must not be repetitions in the list.  
  -> [a] 
  -> [[Integer]]
getHashes2 selmarkNum ks xs = map (map toNum . filter (not . null) . map (idList ks) .  -- before this mapping the smallest element can potentially have 'orD' equal to 0 or greater than 0. The greatest element has 'orD' equal to @selmarkNum - 1@ (@= periodLength - 1@).
 g [selmarkNum-1,selmarkNum-2..] . sort . 
   zipWith S2 [selmarkNum-1,selmarkNum-2..]) . splitF (fromIntegral selmarkNum) $ xs
     where g (q:qs) xs@(x:ys) = let (js,rs) = span (== x) ys in map (\(S2 k y) -> As3 k q y) (x:js) : g qs rs
           g _ _ = []

-- | Convert hashes into basic simler data to evaluate rhythmicity of the list data.
countHashesPrioritized tss@(ts:vs:xss) = zipWith unionCount ts vs : countHashesPrioritized (vs:xss)
countHashesPrioritized _ = []

-- | Mostly for testing.
count1Hashes 
  :: Ord a => Int8
  -> [Int8]
  -> [a]
  -> Integer
count1Hashes groupLength ks = sum . map createNewHash . countHashesPrioritized . getHashes2 groupLength ws 
    where !ws = sortBy (\x y -> compare y x) . filter (>= 0) $ ks
{-# INLINE count1Hashes #-}

{-| Data type to encode the changes  that are introduced by the position  of the group 
 of values in general sequence to the general result of the 'createHashesG' function. If the second parameter  in the 'HashCorrections' is 1 then the result is more 
 sensitive to beginning of the line; if it is set to 2 then the result is more sensitive
 to ending of the line; if it is greater than 2 then the result is sensitive to some
user weights provided as the first parameter to 'HashCorrections' and otherwise 
the computation result does not depend on the first parameter to 'HashCorrections' (this
one can be considered  the basic option for the computation).
-}
data HashCorrections = H [Int8] Int8 deriving (Eq, Show, Read)

hashCorrections2F :: HashCorrections -> (Int8 -> [Integer] -> Integer)
hashCorrections2F (H _ k) 
 | k > 0  = hashPosLF2
 | k == 0 = hashBalancingLF2
 | otherwise = hashBasicLF2
{-# INLINE hashCorrections2F #-}

{-| If the second parameter  in the 'HashCorrections' is 1 then the result is more 
 sensitive to beginning of the line; if it is set to 2 then the result is more sensitive
 to ending of the line; if it is greater than 2 then the result is sensitive to some
user weights provided as the first parameter to 'HashCorrections' and otherwise 
the computation result does not depend on the first parameter to 'HashCorrections' (this
one can be considered  the basic option for the computation).
-}
hashList :: HashCorrections -> [Int8]
hashList (H _ 1) = [10,9..]
hashList (H _ 2) = [1..21] ++ cycle [0]
hashList (H xs _) = xs ++ cycle [0]
{-# INLINE hashList #-}

-- | General implementation of  the hash-based algorithm to evaluate the level of rhythmicity 
-- of the list data. The relatively greater result (for PhLADiPreLiO) corresponds to greater detected periodicity.
countHashesG 
  :: Ord a => HashCorrections -- ^ Data that specifies how the arguments influence the result. Somewhat the kernel of the 'countHashesG' computation.
  -> Int8 -- ^ The period of the length of the initial list.
  -> [Int8] -- ^ List of ordinary positions of the maximum-minimum levels for values of the list in the group. The length of the unique elements together in the list is expected to be
  -- in the list [1..7]. 
  -> [a]
  -> [Integer]
countHashesG hc groupLength ks  = -- sum . 
  zipWith (createHashG f) positions . countHashesPrioritized . getHashes2 groupLength ws
   where f = hashCorrections2F hc
         positions = hashList hc
         !ws = sortBy (\x y -> compare y x) . filter (>= 0) $ ks
{-# INLINE countHashesG #-}

-- | Provided for testing.
createNewHash :: [Integer] -> Integer
createNewHash (x1:x2:x3:x4:x5:x6:x7:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40, shiftL x6 20, x7]
createNewHash (x1:x2:x3:x4:x5:x6:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40, shiftL x6 20]
createNewHash (x1:x2:x3:x4:x5:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40]
createNewHash (x1:x2:x3:x4:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60]
createNewHash (x1:x2:x3:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80]
createNewHash (x1:x2:_) = sum [shiftL x1 120, shiftL x2 100]
createNewHash (x1:_) = shiftL x1 120
createNewHash _ = 0

-- | General implementation of the second hashing of the data for the algorithm.
createHashG :: (Int8 -> [Integer] -> Integer) -> Int8 -> [Integer] -> Integer
createHashG f pos = f pos . zipWith (\n x -> shift x (n*20)) [6,5..0]
{-# INLINE createHashG #-}

-- | A variant of the 'createHashG' that actually must be equal to the 'createNewHash' for the
-- second argument lists 
-- with less than 8 elements. For greater values is not correctly defined, so do not use it for 
-- the lists with 8 or more elements in them. Actually should be equal to 'createNewHash' for the
-- second argument.
createNHash :: [Int8] -> [Integer] -> Integer
createNHash _ = createNewHash . take 7
{-# INLINE createNHash #-}

-- | Function to filter the elements by the second parameter of the 'ASort3' data 
-- and then to get the first ones.
idList :: Eq a => [Int8] -> [ASort3 a] -> [Int8]
idList orDs ys = map (\(As3 k _ _) -> k) . filter (\(As3 _ n _) -> n `elem` orDs) $ ys

-- | Function to create bitwise representation of the intermediate date for the algorithm.
-- Should be very optimized to run fast.
toNum :: [Int8] -> Integer
toNum xs = foldl' setBit 0 . map (\(I8# k) -> I# k) $ xs

-- | The alternative implementation of the 'toNum' (on the Linux x86_64 for some CPU is 
-- slower than the former one).
toNum2 :: [Int8] -> Integer
toNum2 xs = (sum . map (shiftL 1 . (\(I8# k) -> I# k)) $ xs)::Integer


