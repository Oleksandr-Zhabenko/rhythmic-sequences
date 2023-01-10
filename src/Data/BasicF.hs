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

hashEndingLF2 :: [Integer] -> Maybe Int -> Int -> Int
hashEndingLF2 _ _ n = 6 - 21*n
{-# INLINE hashEndingLF2 #-}

hashBeginningLF2 :: [Integer] -> Maybe Int -> Int -> Int
hashBeginningLF2 _ _ n = n
{-# INLINE hashBeginningLF2 #-}

hashBalancingLF2 :: [Integer] -> Maybe Int -> Int -> Int
hashBalancingLF2 _ _ n = -20*n
{-# INLINE hashBalancingLF2 #-}


