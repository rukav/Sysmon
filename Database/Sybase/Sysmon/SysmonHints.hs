-- |
-- Module      :  SysmonHints
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generate the Sysmon hints (suggestions) by comparing the data from sysmon 
-- report to the corresponding data from the configuration. 

module Database.Sybase.Sysmon.SysmonHints 
       (
         HConfig (..)
       , sysmonHints
       ) where
import Database.Sybase.Sysmon.Log
import Database.Sybase.Sysmon.LogTypes
import Database.Sybase.Sysmon.SysmonTypes
import Text.Printf
import Statistics.Sample
import Control.Monad.Reader
import Data.Vector (fromList)
import Data.ConfigFile
import Data.Either.Utils (forceEither)

type HintEnv a = Reader HConfig a

-- | Sysmon configuration type
data HConfig = HConfig {
  hiCPU :: !Double,
  hiIO :: !Double,
  hiIdle :: !Double,
  hiCheckDiskIO :: !Double,
  loAvgDiskIO :: !Double,
  hiStdDeviation :: !Double,
  hiSwitchPerTransaction :: !Double,
  hiContextSwitchDue :: !Double,
  hiDirtyBuffers :: !Double,
  loCacheHits :: !Double,
  hiCacheWash :: !Double,
  loLargeIO :: !Double,
  hiUlcSemRequests :: !Double,
  hiLogSemRequests :: !Double,  
  hiAvgLogWrites :: !Double,
  hiCommitedTrans :: !Int,
  hiPageSplits :: !Int,
  hiLockSummary :: !Double,
  hiDeadlock :: !Double,
  hiLastPageLock :: !Double,
  hiLockPromotions :: !Int,
  loCacheSpinContention :: !Double,
  ioDelayBy :: !String
}

-- | Default configuration. To override the default configuration item
-- use ConfigFile package API. 
defConfig = forceEither $ do 
  let cp = emptyCP
  cp <- set cp "DEFAULT" "hiCPU" (show 85.0)
  cp <- set cp "DEFAULT" "hiIO" (show 50.0)
  cp <- set cp "DEFAULT" "hiIdle" (show 30.0)
  cp <- set cp "DEFAULT" "hiCheckDiskIO" (show 50.0)
  cp <- set cp "DEFAULT" "loCacheHits" (show 90.0)
  cp <- set cp "DEFAULT" "hiCacheWash" (show 5.0)
  cp <- set cp "DEFAULT" "loLargeIO" (show 90.0)
  cp <- set cp "DEFAULT" "loAvgDiskIO" (show 0.005)
  cp <- set cp "DEFAULT" "hiStdDeviation" (show 30.0)
  cp <- set cp "DEFAULT" "hiSwitchPerTransaction" (show 60.0)
  cp <- set cp "DEFAULT" "hiContextSwitchDue" (show 10.0)
  cp <- set cp "DEFAULT" "hiDirtyBuffers" (show 3.0)
  cp <- set cp "DEFAULT" "hiUlcSemRequests" (show 10.0)
  cp <- set cp "DEFAULT" "hiLogSemRequests" (show 10.0)
  cp <- set cp "DEFAULT" "hiAvgLogWrites" (show 1.0)
  cp <- set cp "DEFAULT" "hiCommitedTrans" (show 50)
  cp <- set cp "DEFAULT" "hiPageSplits" (show 10)
  cp <- set cp "DEFAULT" "hiLockSummary" (show 10.0)
  cp <- set cp "DEFAULT" "hiDeadlock" (show 10.0)
  cp <- set cp "DEFAULT" "hiLastPageLock" (show 10.0)
  cp <- set cp "DEFAULT" "hiLockPromotions" (show 20)
  cp <- set cp "DEFAULT" "loCacheSpinContention" (show 10.0)
  cp <- set cp "DEFAULT" "ioDelayBy" "Disk"
  return cp

-- | Create Sysmon configuration from ConfigParser
mkConfig :: ConfigParser -> HConfig
mkConfig c = let cp = Data.ConfigFile.merge defConfig c in
  HConfig {
    hiCPU = forceEither $ get cp "Sysmon" "hiCPU",
    hiIO = forceEither $ get cp "Sysmon" "hiIO",
    hiIdle = forceEither $ get cp "Sysmon" "hiIdle",
    hiCheckDiskIO = forceEither $ get cp "Sysmon" "hiCheckDiskIO",
    loCacheHits = forceEither $ get cp "Sysmon" "loCacheHits",
    hiCacheWash = forceEither $ get cp "Sysmon" "hiCacheWash",
    loLargeIO = forceEither $ get cp "Sysmon" "loLargeIO",
    loAvgDiskIO = forceEither $ get cp "Sysmon" "loAvgDiskIO",
    hiStdDeviation = forceEither $ get cp "Sysmon" "hiStdDeviation",
    hiSwitchPerTransaction = forceEither $ get cp "Sysmon" "hiSwitchPerTransaction",
    hiContextSwitchDue = forceEither $ get cp "Sysmon" "hiContextSwitchDue",
    hiDirtyBuffers = forceEither $ get cp "Sysmon" "hiDirtyBuffers",
    hiUlcSemRequests = forceEither $ get cp "Sysmon" "hiUlcSemRequests",
    hiLogSemRequests = forceEither $ get cp "Sysmon" "hiLogSemRequests",
    hiAvgLogWrites = forceEither $ get cp "Sysmon" "hiAvgLogWrites",
    hiCommitedTrans = forceEither $ get cp "Sysmon" "hiCommitedTrans",
    hiPageSplits = forceEither $ get cp "Sysmon" "hiPageSplits",
    hiLockSummary = forceEither $ get cp "Sysmon" "hiLockSummary",
    hiDeadlock = forceEither $ get cp "Sysmon" "hiDeadlock",
    hiLastPageLock = forceEither $ get cp "Sysmon" "hiLastPageLock",
    hiLockPromotions = forceEither $ get cp "Sysmon" "hiLockPromotions",
    loCacheSpinContention = forceEither $ get cp "Sysmon" "loCacheSpinContention",
    ioDelayBy = forceEither $ get cp "Sysmon" "ioDelayBy"
  }

eval :: Sysmon -> HintEnv [Hint]
eval s = do
   e <- ask 
   let results = [(id, foldResult [r s e | r <- rs] (&&), action) | 
                  (id, rs, action) <- fs
                 ]
   return [(id,action,facts) | (id, (b, facts), action) <- results, b]

sysmonHints :: ConfigParser -> Sysmon -> [Hint]
sysmonHints cnf s = runReader (eval s) (mkConfig cnf)

percent i t = fromIntegral i / fromIntegral t * 100.0

result :: (LogShow a) => Bool -> [String] -> [a] -> Result
result b fs ys = (b, [f ++ " = " ++ lshow y | (f,y) <- zip fs ys, b])

foldResult :: [Result] -> (Bool -> Bool -> Bool) -> Result
foldResult ds cond = foldr1 f ds where
    f (b, s) (b', s') = (cond b b', if b then s ++ s' else s') 

-- | Kernel contention 
checkCpuBusy s e = 
     let busy = avgCpuBusy $ kernel s 
         b = busy >= hiCPU e in
       result b ["CPU Busy"] [busy]

checkCpuIdle s e = 
     let busy = avgCpuBusy $ kernel s 
         b = busy <= 100.0 - hiCPU e in
       result b ["CPU Idle"] [100.0 - busy]

checkIOEngine s e = 
     let iobusy = avgIOBusy $ kernel s
         cpubusy = avgCpuBusy $ kernel s
         b = iobusy >= hiIO e && cpubusy <= (100.0 - hiCPU e) in
       result b ["Engine IO Busy", "Engine CPU Busy"] [iobusy, cpubusy]

checkIODisk s e =
    let checkIO = checkDiskIO $ kernel s
        avgIO = avgDiskIO $ kernel s
        b = checkIO >= hiCheckDiskIO e && avgIO <= loAvgDiskIO e in
     result b ["Check Disk IO", "Average Disk IO"] [checkIO, avgIO]

-- | Task contention
checkEngineBalance s e =
    let ts = taskSwitch $ task s
        sd = stdDev $ fromList $ map (fromIntegral.numSwitch) ts
        b = sd >= hiStdDeviation e in
     result b ["Standard deviation"] [sd]

checkSwitchesPerTran s e =
    let ts = totSwitch $ task s
        tr = commited $ transaction s
        pt = if tr == 0 then 0 
               else fromIntegral ts / fromIntegral tr
        b = pt >= hiSwitchPerTransaction e in
      result b ["Context switches per transaction"] [pt]

checkContextSwitches s e = 
     let sw = taskSwitchDue $ task s
         cs = [(cacheSearchMiss, "Cache Search Misses"), 
               (diskWrites, "System Disk Writes"), 
               (batchSize, "Exceeding I/O batch size"), 
               (logicLockCont, "Logical Lock Contention"),
               (addrLockCont, "Address Lock Contention"), 
               (latchCont, "Latch Contention"), 
               (semCont, "Log Semaphore Contention"), 
               (plcLockCont, "PLC Lock Contention"),
               (lastLogPage, "Last Log Page Writes"), 
               (conflicts, "Modify Conflicts"), 
               (deviceCont, "I/O Device Contention"), 
               (netSent, "Network Packet Sent"),
               (netServices, "Network services"), 
               (other, "Other Causes")
              ]

         conv (f, x) = let res = percent (f sw) (totSwitchDue sw) in (x, res)
         rs = unzip $ filter (\(_,res) -> res >= hiContextSwitchDue e) $ 
                               map conv cs
         b = not $ null $ fst rs in
       uncurry (result b) rs
     
-- | Transaction contention 
checkUlcCont s e = 
     let req = ulsSemReqs $ transaction s
         cont = percent (waited req) (totReq req) 
         b = cont >= hiUlcSemRequests e in
      result b ["ULC Semaphore Contention"] [cont]

checkLogCont s e = 
     let req = logSemReqs $ transaction s
         cont = percent (waited req) (totReq req) 
         b = cont >= hiLogSemRequests e in
      result b ["Log Semaphore Contention"] [cont]

checkAvgLogWrites s e =
     let trans = commited $ transaction s
         avgWrites = avgLogWrites $ transaction s
         b = trans >= hiCommitedTrans e && avgWrites >= hiAvgLogWrites e in
      result b ["Avg # Writes per Log Page"] [avgWrites]

-- | Index contention 
checkPageCont s e = 
     let cont = splits $ index s 
         b = cont >= hiPageSplits e in
      result b ["Page Splits"] [cont]

-- | Lock contention 
checkLockCont s e = 
     let cont = percent (lockCont $ lock s) (lockReqs $ lock s) 
         b = cont >= hiLockSummary e in
      result b ["Average Lock Contention"] [cont]

checkDeadlocks s e =
     let cont = percent (deadlocks $ lock s) (lockReqs $ lock s)
         b = cont >= hiDeadlock e in
       result b ["Deadlock Percentage"] [cont]

checkLastPageLocks s e =
      let cont = percent (waited $ lpLock $ lock s) (totReq $ lpLock $ lock s)
          b = cont >= hiLastPageLock e in
       result b ["Last Page Lock"] [cont]

checkLockPromotion s e =
      let b = promotions (lock s) >= hiLockPromotions e in
       result b ["Lock Promotions"] [promotions $ lock s]
     
-- | Cache contention 
verifyNamedCache cache field valid msg  = 
     let res c = (cacheName c, field c) 
         ps = map res (caches cache) 
         cs = unzip $ filter (\(_, val) -> valid val) ps
         b = not $ null $ fst cs in
       result b (map (++ msg) $ fst cs) (snd cs)

checkCacheTurnover s e =
      let db = (dirtyBuffers.cache) s
          b = db >= hiDirtyBuffers e in
        result b ["Buffers Grabbed Dirty"] [db] 

checkSpinCont s e =
      let valid val = val > 0 && val <= loCacheSpinContention e in
        verifyNamedCache (cache s) spinContention valid ".Spinlock contention"       

checkCacheHits s e =
      let valid val = val > 0 && val <= loCacheHits e 
          field c = percent (hits c) (totHitsMiss c) in  
         verifyNamedCache (cache s) field valid ".Hits"

checkCacheWash s e =
      let valid val = val >= hiCacheWash e
          field c = percent (wash c) (totHitsMiss c) in  
         verifyNamedCache (cache s) field valid ".Wash"

checkCacheLargeIO s e =
       let valid val = val <= loLargeIO e 
           field c = percent (largeIO c) (largeIOTotal c) in  
         verifyNamedCache (cache s) field valid ".Large IO"

-- | Group checks
checkIOBusy s e =
     let checkset = [checkIOEngine, checkIODisk] in 
       foldResult [r s e | r <- checkset] (&&)  

checkResourceCont s e =
      let checkset = [checkContextSwitches, checkSpinCont, checkUlcCont, 
                     checkLogCont, checkPageCont, checkLockCont] in 
       foldResult [r s e | r <- checkset] (||)  

-- | Sysmon rules       
fs = 
   [
   ("ruleMoreEngines", 
     [checkCpuBusy, checkResourceCont], 
     "Consider to increase the number of engines"),
   ("ruleLessEngines", 
     [checkCpuIdle], 
     "Consider to decrease the number of engines"),
   ("rulesEnginesBalance", 
     [checkEngineBalance], 
     "Engines are unbalanced regarding task context switches"),
   ("ruleSwitchPerTransaction", 
     [checkSwitchesPerTran], 
     "The increasing number of the context switches  per transaction"),
   ("ruleContextSwitchDue", 
     [checkContextSwitches], 
     "Consider resources tuning to decrease the context switches due to"),
   ("ruleIOBusy", 
     [checkIOBusy], 
     "The job is IO bound"),
   ("ruleCacheContention", 
    [checkCacheTurnover, 
     checkSpinCont, 
     checkCacheHits, 
     checkCacheWash, 
     checkCacheLargeIO], 
    "Cache contention"),
   ("ruleLockContention", 
     [checkLockCont, 
      checkDeadlocks, 
      checkLastPageLocks, 
      checkLockPromotion], 
     "Lock contention")
   ]

