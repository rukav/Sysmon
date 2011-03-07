-- |
-- Module      :  Log
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The module provides the generic api to parse the logs, to store the parsed
-- data in the IntervalMap and to query data from the IntervalMap based on the
-- given time interval. 
--   

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
import Data.Time
import Data.Maybe (fromMaybe)
import Data.ConfigFile hiding (merge)
import Control.Monad.State (forM)
import qualified System.FilePath.Glob as G (compile, globDir1) 
import System.FilePath (splitFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.PrettyPrint
import Database.Sybase.Sysmon.LogTypes
import Database.Sybase.Sysmon.Average

-- | The request time interval to query sysmon reports. 
-- If the value of the request interval is Nothing the default max time
-- interval request will be used. See function maxInterval below.
type LogRequest = Maybe LogInterval

-- | Merge two log trees
merge :: LogEntry a => LogTree a -> LogTree a -> LogTree a
merge = union

-- | Generic parse the log files and store the data in the log tree. 
-- To parse sysmon logs use parseSysmon from SysmonLog package.
-- This package implements Sysmon instance of LogEntry class (see Sample.hs)
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

-- | Max interval to cover all intervals in the log tree 
maxInterval = mkInterval startOfTime (unsafePerformIO getCurrentTime)
   where startOfTime = buildTime defaultTimeLocale []

-- | Get hints for the average sysmon report corresponding to the request 
-- time interval. To override the default hints parameters use ConfigFile
-- api. See HConfig data type in SysmonTypes package for the list of the
-- configuartion parameters.
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

-- | Average sysmon report corresponding to the requested time interval
average :: (Averageable a, LogEntry a) => LogRequest -> LogTree a -> a        
average x = avg . list x

-- | Get log reports which intersecs with the requested time interval
list :: LogEntry a => LogRequest -> LogTree a -> [a]
list x = map (\(LogNode x) -> snd x) . find (fromMaybe maxInterval x)

-- | Get intervals which intersect with the requested interval 
intervals :: LogEntry a => LogRequest -> LogTree a -> [LogInterval]
intervals x = map (\(LogNode x) -> fst x) . find (fromMaybe maxInterval x)

-- | Check if the log tree contains an interval corresponding to the
-- requested time interval
hasInterval :: LogEntry a => LogRequest -> LogTree a -> Bool
hasInterval x = not . null . intervals x

-- | Create log time interval
mkInterval :: UTCTime -> UTCTime -> LogInterval
mkInterval = Interval

-- | All intervals that intersect with the given interval, in 
-- lexicographical order
find :: LogEntry a => LogInterval -> LogTree a -> [LogNode a]
find x = map LogNode . intersections x



