{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}

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
  :: Ord a => Int8 
  -> [Int8] -- ^ The list must be sorted in the descending order, the elements must be greater than -1 and there must not be repetitions in the list.  
  -> [a] 
  -> [[Integer]]
getHashes2 groupLength ks xs = map (map toNum . filter (not . null) . map (idList ks) .  g [groupLength-1,groupLength-2..] . sort . 
   zipWith S2 [groupLength-1,groupLength-2..]) . splitF (fromIntegral groupLength) $ xs
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

-- | General implementation of  the hash-based algorithm to evaluate the level of rhythmicity 
-- of the list data. The greater result corresponds to greater detected periodicity in the 
-- relative values.
countHashesG 
  :: Ord a => ([Integer] -> Int -> Int -> Int) -- ^ Function that specifies how the arguments influence the result. Somewhat the kernel of the 'countHashesG' computation. The second argument is taken from the second argument being 'zipWith' the 'createHashG' function.
  -> [Int] -- ^ The list  of parameters that if 'Just' @n@ (n>0) then is expected to be the 'Just' position (analogue of an index) of the group in the general  sequence. If set to [] that should mean no dependency on the position of the group in the general sequence exists (looks the simpler case than the former one). Otherwise, the count of positions starts from the 1 and goes further @(2, 3,.., k)@.
  -> Int8 -- ^ Is expected to be from the list @[6, 5,.., 0]@. Is the number of explicitly expected markers to be computed.
  -> [Int8] -- ^ List of ordinary positions of the maximum-minimum levels for values of the list in the group. The length of the unique elements together in the list is expected to be
  -- in the list [1..7]. 
  -> [a]
  -> Integer
countHashesG f positions groupLength ks  = sum . zipWith (\pos vals -> createHashG positions f vals) positions . countHashesPrioritized . getHashes2 groupLength ws
   where !ws = sortBy (\x y -> compare y x) . filter (>= 0) $ ks
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
createHashG :: [Int] -> ([Integer] -> Int -> Int -> Int) -> [Integer] -> Integer
createHashG positions f xs = sum . zipWith3 (\x pos n -> shift x (n*20 + f xs pos n)) xs positions $ [6,5..]

-- | A variant of the 'createHashG' that actually must be equal to the 'createNewHash' for the
-- second argument lists 
-- with less than 8 elements. For greater values is not correctly defined, so do not use it for 
-- the lists with 8 or more elements in them.
createNHash :: [Int] -> [Integer] -> Integer
createNHash positions = createHashG positions (\_ _ _ -> 0)
{-# INLINE createNHash #-}

-- | Similar to 'createNHash' limitations. Gives greater values for the last elements of the list
-- than the former one.
createHashEndingL :: [Int] ->  [Integer] -> Integer
createHashEndingL positions = createHashG positions hashEndingLF2
{-# INLINE createHashEndingL #-}

-- | Similar to 'createNHash' limitations. Gives greater values for  the first elements of the list
-- than the former one.
createHashBeginningL :: [Int] -> [Integer] -> Integer
createHashBeginningL positions = createHashG positions hashBeginningLF2
{-# INLINE createHashBeginningL #-}

-- | Similar to 'createNHash' limitations. Treats all the list elements as equally important
-- to influence the result.
createHashBalancingL :: [Int] -> [Integer] -> Integer
createHashBalancingL positions = createHashG positions hashBalancingLF2
{-# INLINE createHashBalancingL #-}

-- | Similar to 'createNHash' limitations. Treats all the list elements as equally important
-- to influence the result.
createHashBasicL :: [Int] -> [Integer] -> Integer
createHashBasicL positions = createHashG positions hashBasicLF2
{-# INLINE createHashBasicL #-}

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


