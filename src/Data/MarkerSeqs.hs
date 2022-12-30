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

import GHC.Base
import GHC.List
import GHC.Show
import Data.Bits
import Numeric (showIntAtBase,showInt)
import Data.Foldable

showBin :: Int -> [Char]
showBin x = '0':'b':showIntAtBase 2 (head . flip showInt "") x ""
{-# INLINE showBin #-}

unionCount :: (Bits a) => a -> a -> Int
unionCount x = popCount . (.&.) x
{-# INLINE unionCount #-}

countWeightsQs :: (Foldable t) => [t a -> Int] -> [t a] -> [[Int]]
countWeightsQs fs xs = map (flip map xs) fs

