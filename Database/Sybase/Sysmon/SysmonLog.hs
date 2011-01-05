-- |
-- Module      :  SysmonLog
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parse Sybase 15 Sysmon report 

module Database.Sybase.Sysmon.SysmonLog 
       ( parseSysmon
       ) where
import Database.Sybase.Sysmon.LogTypes
import Database.Sybase.Sysmon.LogParserPrim
import Database.Sybase.Sysmon.Log (parse, Interval(..))
import Database.Sybase.Sysmon.SysmonTypes
import Database.Sybase.Sysmon.SysmonHints
import Control.Monad.State
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.DateTime

instance LogEntry Sysmon where
  mkNode s = LogNode (sysmonTime s, s)
  mkParse str = evalState getSysmon (lines str)
  mkHints = sysmonHints

parseSysmon :: FilePath -> IO (LogTree Sysmon)
parseSysmon = parse

getTimeInterval :: LogState LogInterval
getTimeInterval = do
   start <- matchLine "Sampling Started at:"
   end <- matchLine "Sampling Ended at:"
   let parse = fromJust . parseDateTime "%b %d, %Y %H:%M:%S" . concat . drop 3
   return $ Interval (parse start) (parse end)

getKernel :: LogState Kernel
getKernel = do
   goto ["Kernel Utilization", "Engine Busy Utilization", "-"]
   engBusy <- whileJust getEngineBusy

   avg <- matchLine "Average"
   let avgCpu = field 1 avg
   let avgIO = field 3 avg

   goto ["CPU Yields by Engine", "-"]
   cpuYlds <- whileJust getCpuYield
   let totYlds = sum $ map yields cpuYlds

   chk <- matchLine "Checks Returning I/O"
   let checkDiskIO = field 6 chk

   avd <- matchLine "Avg Disk I/Os Returned"
   let avgDiskIO = field 6 avd

   return $ Kernel engBusy cpuYlds avgCpu avgIO totYlds checkDiskIO avgDiskIO

getEngineBusy :: LogState (Maybe EngineBusy)
getEngineBusy = do
   b <- look "Engine " "CPU Yields by Engine"
   if b then do 
        eng <- matchLine "Engine "
        let name = string 0 2 eng
        let cpu = field 2 eng
        let io = field 4 eng
        return $ Just $ EngineBusy name cpu io
     else return Nothing

getCpuYield :: LogState (Maybe CpuYield)
getCpuYield = do
   b <- look "Engine " "Network Checks"
   if b then do 
        eng <- matchLine "Engine "
        let engName = string 0 2 eng 
        let yields = field 4 eng
        return $ Just $ CpuYield engName yields
     else return Nothing

getTask :: LogState Task
getTask = do
  goto ["Task Management"]
  con <- matchField "Connections" 4

  taskSwitch <- whileJust getTaskSwitch
  switchDue <- getTaskSwitchDue

  let totSwitch = sum $ map numSwitch taskSwitch
  return $ Task con taskSwitch switchDue totSwitch

getTaskSwitch :: LogState (Maybe TaskSwitch)
getTaskSwitch = do
   b <- look "Engine " "Total Task Switches"
   if b then do 
        eng <- matchLine "Engine "
        let name = string 0 2 eng
        let count = field 4 eng
        return $ Just $ TaskSwitch name count
     else return Nothing

getTaskSwitchDue :: LogState TaskSwitchDue
getTaskSwitchDue = do
  yields <- matchField "Voluntary Yields" 4
  misses <- matchField "Cache Search Misses" 5
  batch  <- matchField "Exceeding I/O batch size" 6
  disk   <- matchField "System Disk Writes" 5
  lcont  <- matchField "Logical Lock Contention" 5
  acont  <- matchField "Address Lock Contention" 5
  latch  <- matchField "Latch Contention" 4
  sem    <- matchField "Log Semaphore Contention" 5
  plc    <- matchField "PLC Lock Contention" 5
  sleeps <- matchField "Group Commit Sleeps" 5
  last   <- matchField "Last Log Page Writes" 6
  confts <- matchField "Modify Conflicts" 4
  device <- matchField "I/O Device Contention" 5
  rcvd   <- matchField "Network Packet Received" 5
  sent   <- matchField "Network Packet Sent" 5
  srvs   <- matchField "Network services" 4
  other  <- matchField "Other Causes" 4

  let totSwitchDue = yields + misses + batch + disk + lcont + acont +
                     latch + sem + plc + sleeps + last + confts + device +
                     rcvd + sent + srvs + other

  return $ TaskSwitchDue yields misses batch disk lcont acont latch sem plc
                         sleeps last confts device rcvd sent srvs other totSwitchDue

getTransaction :: LogState Transaction
getTransaction = do
  goto ["Transaction Profile"]

  commits <- optField "Committed Xacts" "Transaction Detail" 4 0
  inserts <- matchField "Total Rows Inserted" 5
  updates <- matchField "Total Rows Updated" 5
  deletes <- matchField "Total Rows Deleted" 5
  flushes <- getUlcFlush 
 
  ulsReqs <- getRequest "ULC Semaphore Requests"
  logReqs <- getRequest "Log Semaphore Requests"

  logWrites <- optField "Avg # Writes per Log Page" "Index Management" 8 0
  return $ Transaction commits inserts updates deletes flushes ulsReqs logReqs logWrites

getUlcFlush :: LogState UlcFlush
getUlcFlush = do
  ulc <- matchField "by Full ULC" 5
  tran <- matchField "by End Transaction" 5
  db <- matchField "by Change of Database" 6
  log <- matchField "by Single Log Record" 6
  unpin <- matchField "by Unpin" 4
  other <- matchField "by Other" 4

  let totFlush = ulc + tran + db + log + unpin + other
  return $ UlcFlush ulc tran db log unpin other totFlush
 
getRequest :: String -> LogState Request
getRequest label = do
  goto [label]
  granted <- optField "Granted" "Total" 3 0
  waited <- optField "Waited" "Total" 3 0
  return $ Request granted waited (granted + waited)
  
getIndex :: LogState Index
getIndex = do
  goto ["Index Management"]
  splits <- matchField "Page Splits" 4
  shrinks <- matchField "Page Shrinks" 4
  return $ Index splits shrinks

getLock :: LogState Lock
getLock = do
  goto ["Lock Management"]
  lockReqs <- matchField "Total Lock Requests" 5
  lockCont <- matchField "Avg Lock Contention" 5
  dlocks <- matchField "Deadlock Percentage" 4
  exTable <- getRequest "Exclusive Table"
  shTable <- getRequest "Shared Table"
  exIntent <- getRequest "Exclusive Intent"
  shIntent <- getRequest "Shared Intent"
  exPage <- getRequest "Exclusive Page"
  upPage <- getRequest "Update Page"
  shPage <- getRequest "Shared Page"
  exRow <- getRequest "Exclusive Row"
  upRow <- getRequest "Update Row"
  shRow <- getRequest "Shared Row"
  exAddress <- getRequest "Exclusive Address"
  shAddress <- getRequest "Shared Address"
  lpLock <- getRequest "Last Page Locks on Heaps"

  prom <- matchField "Total Lock Promotions" 5
  tout <- matchField "Total Timeouts" 4

  return $ Lock lockReqs lockCont dlocks exTable shTable exIntent shIntent exPage
                upPage shPage exRow upRow shRow exAddress shAddress lpLock prom tout

getCache :: LogState Cache
getCache = do  
  hits <- optField "Total Cache Hits" "Total Cache Searches" 5 0
  misses <- optField "Total Cache Misses" "Total Cache Searches" 5 0 
  dirtyBuffers <- optField "Buffers Grabbed Dirty" "Cache Strategy Summary" 6 0
  caches <- whileJust getNamedCache

  return $ Cache hits misses dirtyBuffers (hits+misses) caches --totCache caches

getNamedCache :: LogState (Maybe NamedCache)
getNamedCache = do
  cache <- optString "Cache: " "Procedure Cache Management" 1 2 ""
  if null cache then return Nothing
   else do    
    spin <- matchField "Spinlock Contention" 5
    util <- matchField "Utilization" 4 
    hits <- optField "Cache Hits" "Total Cache Searches" 4 0
    wash <- optField "Found in Wash" "Total Cache Searches" 5 0
    miss <- optField "Cache Misses" "Total Cache Searches" 4 0
    perf <- optField "Large I/Os Performed" "Total Large I/O Requests" 5 0
    reqs <- matchField "Total Large I/O Requests" 6    
    return $ Just $ NamedCache cache spin util hits wash miss (hits + miss) perf reqs

getDisk :: LogState Disk
getDisk = do
    goto ["Disk I/O Management"]
    engIO <- whileJust getEngineIO

    diskIO <- matchField "Disk I/O Structures" 5
    server <- matchField "Server Config Limit" 5
    engine <- matchField "Engine Config Limit" 5
    os <- matchField "Operating System Limit" 5
    reqIO <- matchField "Total Requested Disk I/Os" 6

    goto ["-"]
    comIO <- matchField "Total Completed I/Os" 5
    dev <- whileJust getDevice

    return $ Disk engIO diskIO server engine os reqIO comIO dev

getEngineIO :: LogState (Maybe EngineIO)
getEngineIO = do
    b <- look "Engine " "I/Os Delayed by"
    if b then do 
        engIO <- matchLine "Engine "
        let name = string 0 2 engIO
        let outIO = field 4 engIO
        return $ Just $ EngineIO name outIO
     else return Nothing

getDevice :: LogState (Maybe Device)
getDevice = do
     name <- optString "Device: " "Network I/O Management" 0 1 ""
     if null name then return Nothing
      else do    
        totIO <- matchField "Total I/Os" 4
        return $ Just $ Device name totIO

getSysmon :: LogState Sysmon
getSysmon = do
   time <- getTimeInterval
   kernel <- getKernel
   task <- getTask
   transaction <- getTransaction
   index <- getIndex
   lock <- getLock
   cache <- getCache
   disk <- getDisk

   return $ Sysmon time kernel task transaction 
                   index lock cache disk 

















