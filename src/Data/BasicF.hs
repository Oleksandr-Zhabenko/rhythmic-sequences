{-# LANGUAGE BinaryLiterals, NoImplicitPrelude, Strict #-}

-- |
-- Module      :  Data.BasicF
-- Copyright   :  (c) Oleksandr Zhabenko 2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- 
module Data.BasicF where

import Data.Bits
import GHC.Num
import GHC.Float
import GHC.Base
import GHC.Real
import Data.Bool
import GHC.List
import GHC.Show

hashEndingLF2 :: [Integer] -> Int -> Int -> Int
hashEndingLF2 _ k n = 6 - 21*n + k
{-# INLINE hashEndingLF2 #-}

hashBeginningLF2 :: [Integer] -> Int -> Int -> Int
hashBeginningLF2 _ k n = n + 30 - k
{-# INLINE hashBeginningLF2 #-}

hashBalancingLF2 :: [Integer] -> Int -> Int -> Int
hashBalancingLF2 _ _ n = -20*n
{-# INLINE hashBalancingLF2 #-}

hashBasicLF2 ::  [Integer] -> Int -> Int -> Int
hashBasicLF2 _ _ _ = 0
{-# INLINE hashBasicLF2 #-}


