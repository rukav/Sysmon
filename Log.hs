module Log (
Interval (..), LogRequest, merge,
parse, hints, average, list,
intervals, hasInterval, mkInterval
) where

import Data.IntervalMap.FingerTree
import Data.DateTime
import Data.Maybe (fromMaybe)
import Data.ConfigFile hiding (merge)
import Control.Monad.State (forM)
import qualified System.FilePath.Glob as G (compile, globDir1) 
import System.FilePath (splitFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import LogTypes
import Average

find :: (LogEntry a) => LogInterval -> LogTree a -> [LogNode a]
find x = map LogNode . intersections x

-- API
type LogRequest = Maybe LogInterval

merge :: LogEntry a => LogTree a -> LogTree a -> LogTree a
merge = union

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

maxInterval = mkInterval startOfTime (unsafePerformIO getCurrentTime)

hints :: (Averageable a, LogEntry a) => LogRequest -> ConfigParser -> LogTree a -> [Hint]
hints x cp = mkHints cp . average x 

average :: (Averageable a, LogEntry a) => LogRequest -> LogTree a -> a        
average x = avg . list x

list :: LogEntry a => LogRequest -> LogTree a -> [a]
list x = map (\(LogNode x) -> snd x) . find (fromMaybe maxInterval x)

intervals :: LogEntry a => LogRequest -> LogTree a -> [LogInterval]
intervals x = map (\(LogNode x) -> fst x) . find (fromMaybe maxInterval x)

hasInterval :: LogEntry a => LogRequest -> LogTree a -> Bool
hasInterval x = not . null . intervals x

mkInterval :: DateTime -> DateTime -> LogInterval
mkInterval = Interval



