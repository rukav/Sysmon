import Data.ConfigFile
import Data.DateTime
import Log
import LogTypes
import SysmonLog
import System.IO

main = do
  tree <- parseSysmon "tests/sysmon_100310_0952.out"
  print $ "Sysmon log has interval: " ++ show (hasInterval interval tree)
  print $ "Sysmon log intervals: " ++ show (intervals interval tree)
  print $ fmtHints $ hints Nothing emptyCP tree  

interval :: LogRequest
interval = Just $ mkInterval start end where
   y  = 2010 :: Integer
   start = fromGregorian y 3 10 09 52 37
   end   = fromGregorian y 3 10 09 53 37


