{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Average
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class to calculate the average Sysmon report 

module Average where
import LogTypes
import Data.List
import Data.IntervalMap.FingerTree

class Averageable a where
  avg :: [a] -> a

instance Averageable Int where
  avg = fst . foldl' (\(!m, !n) xs -> (m+(xs-m) `div` (n+1),n+1)) (0,0) 

instance Averageable Double where
  avg = fst . foldl' (\(!m, !n) xs -> (m+(xs-m)/(n+1),n+1)) (0,0)

instance Averageable String where
  avg [] = ""
  avg xs = head xs

instance Averageable LogInterval where
  avg xs = Interval s e where
   s = foldr1 min $ map low xs
   e = foldr1 max $ map high xs
