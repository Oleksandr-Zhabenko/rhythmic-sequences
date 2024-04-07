{-# LANGUAGE NoImplicitPrelude, BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Rhythmicity.MarkerSeqs2
-- Copyright   :  (c) Oleksandr Zhabenko 2022-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Data and algorithmic basics to evaluate rhythmicity of the lists of 'Ord' instance data type.
-- Similar to @phonetic-languages-rhythmicity@ on Hackage.

module Rhythmicity.MarkerSeqs2 where

import GHC.Num
import GHC.Real
import GHC.Base
import Data.List hiding (foldr)
import GHC.Int
import Rhythmicity.BasicF
import Data.Tuple (fst,snd)
import Data.List.Split (chunksOf)  
import Rhythmicity.MarkerSeqs  hiding (getHashes2,countHashesPrioritized,countHashesG,countHashes2G) 

-- | Function to get basic data for hash-based evaluation of the rhythmicity of the list data. Is
-- used internally in the 'countHashesG'.
-- Provided here mostly for testing purposes.
getHashes2 
  :: Ord a => Int8 -- ^ A period of the groups (the length of the lists into which the general sequence is splitted at first).
  -> [Int8] -- ^ The list must be sorted in the descending order, the elements must be greater or equal to 0 and less than the first argument of 'getHashes2' here; besides, there must not be repetitions (any duplicates) in the list so all the elements must be pairwise not equal.  
  -> [a] -- ^ A list of 'Ord' data values that is evaluated for its rhythmic properties.
  -> [[Integer]]
getHashes2 selmarkNum ks xs = map (map toNum . filter (not . null) . map (idList ks) . -- before this mapping the smallest element can potentially have 'orD' equal to 0 or greater than 0. The greatest element has 'orD' equal to @selmarkNum - 1@ (@= periodLength - 1@).
 g sqNum . sort . zipWith S2 sqNum) . chunksOf (fromIntegral selmarkNum) $ xs
     where sqNum = [selmarkNum-1,selmarkNum-2..]
           g (q : qs) (x : ys) = let (js,rs) = span (== x) ys in map (\(S2 k y) -> As3 k q y) (x : js) : g qs rs
           g _ _ = [] 

-- | Convert hashes into basic simler data to evaluate rhythmicity of the list data.
countHashesPrioritized (ts : vs : xss) = zipWith unionCount ts vs : countHashesPrioritized (vs : xss)
countHashesPrioritized _ = []

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
         !ws = sortOn (*(-1)) . filter (>= 0) $ ks
{-# INLINE countHashesG #-}

-- | General implementation of  the hash-based algorithm to evaluate the level of rhythmicity 
-- of the list data. The relatively greater result (for PhLADiPreLiO) corresponds to greater detected periodicity.
countHashes2G 
  :: Ord a => Int -- ^ The first parameter for 'createHash2G' function — the step of hashing shift. For 'countHashesG' it is equal to 20 (the default sensible value). Is expected to be greater than 2.
  -> HashCorrections -- ^ Data that specifies how the arguments influence the result. Somewhat the kernel of the 'countHashesG' computation.
  -> Int8 -- ^ The period of the length of the initial list.
  -> [Int8] -- ^ List of ordinary positions of the maximum-minimum levels for values of the list in the group. The length of the unique elements together in the list is expected to be
  -- in the list [1..7]. 
  -> [a]
  -> [Integer]
countHashes2G k hc groupLength ks  = -- sum . 
  zipWith (createHash2G k hc) positions . countHashesPrioritized . getHashes2 groupLength ws
   where positions = hashList hc
         !ws = sortOn (*(-1)) . filter (>= 0) $ ks
{-# INLINE countHashes2G #-}

