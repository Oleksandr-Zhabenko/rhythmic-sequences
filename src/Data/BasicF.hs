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

hashEndingLF :: [Integer] -> Int -> Int
hashEndingLF _ n = 6 - 21*n
{-# INLINE hashEndingLF #-}

hashBeginningLF :: [Integer] -> Int -> Int
hashBeginningLF _ n = n
{-# INLINE hashBeginningLF #-}

hashBalancingLF :: [Integer] -> Int -> Int
hashBalancingLF _ n = -20*n
{-# INLINE hashBalancingLF #-}

