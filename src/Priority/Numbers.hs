{-# LANGUAGE BinaryLiterals, NoImplicitPrelude, Strict, StrictData #-}

-- |
-- Module      :  Priority.Numbers
-- Copyright   :  (c) Oleksandr Zhabenko 2022
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Special numbers with just one hopefully fast enough operation for some computations. 
-- They provide some kind of prioritization logics in the addition in the numbers 
-- in the several directions (somewhat like a multidimensional ordering).

module Priority.Numbers where

import Data.Bits
import GHC.Num
import GHC.Float
import GHC.Base
import GHC.Real
import Data.Bool
import GHC.List
import GHC.Show


{-| Specifically optimized numbers with the greatest priority at the beginning of the list inside.
   Please, do not use if not created with 'list2PriorityNum' or checked with 'validPriorityNum' before 
   or you are not sure that the length of the list is not less than 2 and the 'dimensionality' equals
   to it.
-}
data PriorityNum = PN {
  dimensionality :: Int,
  nums :: [Integer]
} deriving Show

list2PriorityNum :: [Integer] -> Maybe PriorityNum
list2PriorityNum xs@(x:y:ys) = let l = length xs in Just (PN l xs)
list2PriorityNum _ = Nothing
{-# INLINE list2PriorityNum #-}

validPriorityNum :: PriorityNum -> Bool
validPriorityNum x@(PN k ys) 
 | l > 1 = k == l
 | otherwise = False
  where l = length ys
{-# INLINE validPriorityNum #-}

priorityNum2List :: PriorityNum -> [Integer]
priorityNum2List (PN _ xs) = xs
{-# INLINE priorityNum2List #-}

instance Eq PriorityNum where
  PN k1 ws@(x:xs) == PN k2 ts@(y:ys)
    | k1 == k2 = x == y && xs == ys
    | otherwise = False

instance Ord PriorityNum where
  compare (PN k1 xs@(t:w:ts)) (PN k2 ys@(u:v:us))
     | k1 /= k2 = compare k1 k2
     | t /= u = compare t u
     | w /= v = compare w v
     | otherwise = compare ts us

-- | Please, be aware of the possible 'Integer' buffer overflow and segmentation faults while
-- using with rather big enough second argument list elements. But for several thousands (tested at least 
-- for 10240 on the Linux x86_64 architecture) it works.
changePriorityNum :: PriorityNum -> [Int] -> PriorityNum
changePriorityNum (PN k xs) = PN k . zipWith shift xs
{-# INLINE changePriorityNum #-}

-- | 
-- 
-- > test2 . changePriorityNum (PN 5 [2788,24772,7373,23,222]) $ [0,1,-4,5,6]
-- > "PN 5 [2788,49544,460,736,14208]"
--
test2 :: PriorityNum -> String
test2 (PN k xs) = "PN " ++ show k ++ ' ':show xs

defPN :: Int -> PriorityNum
defPN k = PN k . replicate k $ 0b10000000000000000000000000000000

-- | Please, be aware of the possible 'Integer' buffer overflow and segmentation faults while
-- using with rather big enough second argument. But for several thousands (tested at least 
-- for 10240 on the Linux x86_64 architecture) it works.
defGPN :: Int -> Int -> Maybe PriorityNum
defGPN k n 
 | n > 2 = Just . PN k . replicate k $ p
 | otherwise = Nothing
    where p = shiftL 0b1 n
