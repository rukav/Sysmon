import Data.ConfigFile
import Data.DateTime
import Data.Either.Utils
import Control.Monad

import Log
import LogTypes
import SysmonLog
import SysmonTypes (Sysmon)
--import Cmp
import IO

{--
main = do
  sysmon <- parseSysmon "data/day1/sysmonlogs/sysmon*.out"
  h <- openFile "data/day1/cmplogs/cmp_Main_v1714_tuning_day1_p1.log" ReadMode
  c <- hGetContents h
  let res = jobHints sysmon emptyCP $ bigJobs 5 (jobs $ parseCmp c)
  format res

format res = do
  forM_ res $ \(hints, job) -> do
    let duration = diffSeconds (high $ cmpTime job) (low $ cmpTime job)
    putStrLn $ "Job: " ++ title job ++ " - " ++ show duration ++ " sec"
    putStrLn $ "  Hints:"
    forM_ hints $ \(id, msg, facts) -> do 
      putStrLn $ "    " ++ msg
      putStrLn $ "      " ++ show facts 
      putStrLn $ "        Rule Name: " ++ id  

jobHints :: LogTree Sysmon -> ConfigParser -> [Job] -> [([Hint], Tag)]
jobHints sysmon cp = map f where
  f job = (hints (Just $ cmpTime (jobTag job)) cp sysmon, jobTag job)
--}

main = do
  tree <- parseSysmon "tests/sysmon_100310_0952.out"
{-- 
  val <- readfile emptyCP "/etc/foo.cfg"
  let cp = forceEither val
  mapM_ print $ hints Nothing cp tree
--}
  print $ hints Nothing emptyCP tree  
--  print $ average Nothing tree
  print $ length $ list interval tree
  print $ length $ intervals interval tree
  print $ hasInterval interval tree

interval :: LogRequest
interval = Just $ mkInterval start end where
   y  = 2010 :: Integer
   start = fromGregorian y 3 10 09 52 37
   end   = fromGregorian y 3 10 09 53 37
