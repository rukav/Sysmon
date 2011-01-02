{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module Average where
import Data.List
import LogTypes
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
