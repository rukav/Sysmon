import Data.ConfigFile
import Data.Time
import Database.Sybase.Sysmon.Log
import Database.Sybase.Sysmon.SysmonLog (parseSysmon)
import Database.Sybase.Sysmon.SysmonTypes
import Data.Either.Utils (forceEither)
import Control.Monad (when)

import System.IO
import System.Locale (defaultTimeLocale)

-- | Simple application to parse the sysmon report files,
-- to execute simple queries against parsed data, to generate the
-- the hints potentially to consider. See defConfig in SysmonHints
-- module for the default hints configuration. ConfigFile package api
-- is used to change one of the threshold confifuration parameters
--      
main = do
  tree <- parseSysmon "../data/sysmon_*.out"
  when (hasInterval logRequest tree) $ do
    print $ "Time interval: " ++ show (timeInterval tree)
    print $ "Transactions: " ++ show (numTrans tree)
    print $ fmtHints $ hints Nothing hintsCfg tree   
  where
    timeInterval = sysmonTime . average logRequest
    numTrans = sum . map (commited . transaction) . list logRequest
    hintsCfg = forceEither $ do
         let cp = emptyCP
         cp <- set cp "DEFAULT" "hiContextSwitchDue" "30.0"
         return cp

-- | Request time interval to query
logRequest :: LogRequest
logRequest = Just $ mkInterval start end where
   start = readTime defaultTimeLocale timeFormat "Jan 27, 2010 10:00:00"
   end = readTime defaultTimeLocale timeFormat "Jan 27, 2010 18:00:00"
   timeFormat = "%b %d, %Y %H:%M:%S"


