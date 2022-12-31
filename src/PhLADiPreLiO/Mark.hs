{-# LANGUAGE NoImplicitPrelude, BangPatterns, MultiWayIf #-}

-- |
-- Module      :  PhLADiPreLiO.Mark
-- Copyright   :  (c) Oleksandr Zhabenko 2022
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
--

module PhLADiPreLiO.Mark where

import GHC.Base
import GHC.List
import GHC.Show
import Data.Bits
import Numeric (showIntAtBase,showInt)
import Data.Foldable
import Data.MarkerSeqs
import Data.FList
import Priority.Numbers


gPoly wws (PolyCh (j:js) l) (PolyRhythm (y:ys)) vs us
  | null vs = map (\r -> if | isJI r -> (\q@(J rr) -> I (PolyMs (head wws))) r
                            | otherwise -> r) us
  | y == 0 = map (\r -> if | isJI r -> (\q@(J rr) -> I (PolyMs (head wws))) r
                           | otherwise -> r) us
  | otherwise = let ws = sort vs in case j of
       False -> let !k = ws !! (y - 1) in
                   gPoly (drop 1 wws) (PolyCh js l) (PolyRhythm ys) (filter (> k) vs)
                      (map (\r -> if
                                | isJI r -> (\q@(J rr) -> if
                                              | rr <= k -> I (PolyMs (head wws))
                                              | otherwise -> q) r
                                | otherwise -> r) us)
       _  -> let !k = ws !! (length ws - y) in
                   gPoly (drop 1 wws) (PolyCh js l) (PolyRhythm ys) (filter (< k) vs)
                      (map (\r -> if
                                | isJI r -> (\q@(J rr) -> if
                                        | rr >= k -> I (PolyMs (head wws))
                                        | otherwise -> q) r
                                | otherwise -> r) us)
gPoly wws (PolyCh [] l)  _ vs us = map (\r -> if isJI r then I (PolyMs (head wws)) else r) us
  where  f ch@(PolyCh _ l1) ys@(_:_) =
            let !q = length ys `quot` l1
                rs = take (q * l1) ys in f' ch rs
         f' ch@(PolyCh _ l1) qs@(_:_) = let (ts,zs) = splitAt l1 qs in ts : f' ch zs
         f' _ [] = []

getPolyChRhData
  :: (Ord a) => Char -- ^ The start of the 'RP' 'PolyMarkers' count in case of 'PolyMrks' with 'Char's. The usual one can be \'a\' or \'h\'.
  -> Int -- ^ If the argument is less or equal to 4, then 'Marker4s' is used, if it is greater than 4, then 'PolyMarkers' is used.
  -> PolyChoices -- ^ Data specifies the structure of the period of rhythmicity -- whether maximum or minimum elements are considered and how many syllables costitute the period.
  -> PolyRhythmBasis -- ^ Data specifies the quantities of the syllables on the corresponding levels of importance.
  -> [a]
  -> [[PolyMrks]]
getPolyChRhData c r choice@(PolyCh ts l1) rhythm@(PolyRhythm ys) xs
 | r > 4 && validPolyChRhPair choice rhythm =
    map (\ks -> map (fromJust . fromIntermediate) . gPoly [c..] choice rhythm ks . map J $ ks) . f choice $ xs
 | otherwise = error "PhLADiPreLiO.Mark.getPolyChRhData: the first two arguments cannot be used together to get some meaningful result. "
