-- |
-- Module      :  LogTypes
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic log objects and types.

module LogTypes where
import Data.IntervalMap.FingerTree
import Data.ConfigFile (ConfigParser)
import Data.DateTime
import Text.Printf

-- | Hint is defined as the triple of the rule name, 
-- rule action (text message) and rule conditions
type Hint = (RuleId, Action, Facts)
type Facts = [String]
type Result = (Bool, Facts)
type RuleId = String
type Action = String

-- | LogTree implemented as IntervalMap.FingerTree  
type LogTree a = IntervalMap DateTime a

-- | The key to look for the data in the LogTree
type LogInterval = Interval DateTime

-- | The nodes of the LogTree
data LogNode a = LogNode (LogInterval, a)

-- | Operations to parse log data, make LogTree and generate hints  
class LogEntry a where
  mkNode :: a -> LogNode a
  mkParse :: String -> a
  mkHints :: ConfigParser -> a -> [Hint]
  mkLogTree :: [LogNode a] -> LogTree a
  mkLogTree = foldr ins empty where
     ins (LogNode (i, e)) = insert i e

-- | Format facts data
class (Show a) => LogShow a where
  lshow :: a -> String
  lshow = show

instance LogShow Int
instance LogShow Integer
instance LogShow Bool
instance LogShow Double where
  lshow = printf "%.2f"
