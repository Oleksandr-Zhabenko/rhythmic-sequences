{-# LANGUAGE NoImplicitPrelude, Strict, MagicHash #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BasicF
-- Copyright   :  (c) Oleksandr Zhabenko 2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Functions for Data.MarkerSeqs module that additionally \"catches\" some hashing logics 
-- so that they can influence in the predictable way the peculiarities of the results 
-- for PhLADiPreLiO.

module Data.BasicF where

import Data.Bits
import GHC.Num
import GHC.Base
import GHC.Real
import GHC.List
import GHC.Int

hashPosLF2 :: Int8 -> [Integer] -> Integer
hashPosLF2 k@(I8# i) ns = flip shift (I# i) . sum $ ns 
{-# INLINE hashPosLF2 #-}

hashBalancingLF2 :: Int8 -> [Integer] -> Integer
hashBalancingLF2 _ = sum . zipWith (\pos n -> shiftR n (20*pos)) [6,5..0]
{-# INLINE hashBalancingLF2 #-}

hashBasicLF2 ::  Int8 -> [Integer] -> Integer
hashBasicLF2 _ = sum
{-# INLINE hashBasicLF2 #-}


