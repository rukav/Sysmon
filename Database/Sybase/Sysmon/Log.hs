-- |
-- Module      :  Log
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The generic api to work with the log reports 

module Database.Sybase.Sysmon.Log 
  ( Interval (..)
  , LogRequest
  , merge
  , parse
  , hints 
  , fmtHints
  , average
  , list
  , intervals
  , hasInterval
  , mkInterval
  ) where

import Data.IntervalMap.FingerTree hiding (empty)
import Data.DateTime
import Data.Maybe (fromMaybe)
import Data.ConfigFile hiding (merge)
import Control.Monad.State (forM)
import qualified System.FilePath.Glob as G (compile, globDir1) 
import System.FilePath (splitFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import Text.PrettyPrint
import Database.Sybase.Sysmon.LogTypes
import Database.Sybase.Sysmon.Average

find :: (LogEntry a) => LogInterval -> LogTree a -> [LogNode a]
find x = map LogNode . intersections x

-- | The request to retrieve data from the LogTree. It can be defined:
-- Just (Interval DateTime) - get data corresponding to the requested interval
-- Nothing - get data corresponding to the max interval from the start of time           
--           to the current time (see function maxInterval)
type LogRequest = Maybe LogInterval

-- | Merge two log trees. (Implemented as union function from 
-- IntervalMap.FingerTree package)
merge :: LogEntry a => LogTree a -> LogTree a -> LogTree a
merge = union

-- | Parse the log file(s) to the LogTree.  
-- The LogTree is defined as IntervalMap.FingerTree
parse :: LogEntry a => FilePath -> IO (LogTree a)
parse path = do
    let (dir, fp) = splitFileName path
    fs <- G.globDir1 (G.compile fp) dir
    if null fs then error $ fp ++ ": No such file or directory"
      else do
         mons <- forM fs $ \name -> do
           h <- openFile name ReadMode
           c <- hGetContents h
           return [mkParse c]
         return $ mkLogTree $ map mkNode (concat mons)

-- | Max interval to cover all intervals in the LogTree 
maxInterval = mkInterval startOfTime (unsafePerformIO getCurrentTime)

-- | Get hints for the average sysmon report. To override the default facts constrained
-- use ConfigParser package API. The configuration type (HConfig) and the Sysmon default 
-- configuration parameters (defConfig) are defined in the SysmonHints package.
hints :: (Averageable a, LogEntry a) => LogRequest -> ConfigParser -> LogTree a -> [Hint]
hints x cp = mkHints cp . average x 

-- | Pretty print the hints
fmtHints :: [Hint] -> Doc
fmtHints hs = vcat [text "Hints:", foldr fmtHint empty hs]
  where
    fmtHint (ruleId, msg, details) doc = 
       vcat [doc, 
             vcat [nest 2 (text msg), 
                   nest 4 (text $ show details), 
                   nest 6 (text "Rule Name:" <+> text ruleId) 
                  ]
            ]

-- | Average sysmon report calculated from the reports corresponding
-- to the intersection of the requested interval and LogTree intervals. 
average :: (Averageable a, LogEntry a) => LogRequest -> LogTree a -> a        
average x = avg . list x

-- | Get log reports from LogTree which intervals intersect
-- with the requested interval 
list :: LogEntry a => LogRequest -> LogTree a -> [a]
list x = map (\(LogNode x) -> snd x) . find (fromMaybe maxInterval x)

-- | Get intervals LogTree which intervals intersect
-- with the requested interval 
intervals :: LogEntry a => LogRequest -> LogTree a -> [LogInterval]
intervals x = map (\(LogNode x) -> fst x) . find (fromMaybe maxInterval x)

-- | Check if the requested interval intersects with intervals from LogTree
hasInterval :: LogEntry a => LogRequest -> LogTree a -> Bool
hasInterval x = not . null . intervals x

-- | Create log interval
mkInterval :: DateTime -> DateTime -> LogInterval
mkInterval = Interval



