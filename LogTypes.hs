module LogTypes where
import Data.IntervalMap.FingerTree
import Data.ConfigFile (ConfigParser)
import Data.DateTime

type Hint = (RuleId, Action, Facts)
type Facts = [String]
type Result = (Bool, Facts)
type RuleId = String
type Action = String

type LogInterval = Interval DateTime
data LogNode a = LogNode (LogInterval, a)
type LogTree a = IntervalMap DateTime a

class LogEntry a where
  mkNode :: a -> LogNode a
  mkParse :: String -> a
  mkHints :: ConfigParser -> a -> [Hint]
  mkLogTree :: [LogNode a] -> LogTree a
  mkLogTree = foldr ins empty where
     ins (LogNode (i, e)) = insert i e