{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  SysmonTypes
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Sysmon report types

module SysmonTypes where
import Log
import LogTypes
import Average
import Derive
import Data.List (transpose) 

data EngineBusy = EngineBusy {
  name :: String,
  cpuBusy :: Double,
  ioBusy :: Double
} deriving (Show)

data CpuYield = CpuYield {
  engName :: String,
  yields :: Int
} deriving (Show)

data Kernel = Kernel {
  engBusy :: [EngineBusy],
  cpuYlds :: [CpuYield], 
  avgCpuBusy :: Double,
  avgIOBusy :: Double,  
  totYlds :: Int,
  checkDiskIO :: Double,
  avgDiskIO :: Double
} deriving (Show)

data TaskSwitch = TaskSwitch {
  byEngine :: String,
  numSwitch :: Int
} deriving (Show)

data TaskSwitchDue = TaskSwitchDue {
  volYields :: Int,
  cacheSearchMiss :: Int,
  batchSize :: Int,
  diskWrites :: Int,
  logicLockCont :: Int,
  addrLockCont :: Int,
  latchCont :: Int,
  semCont :: Int,
  plcLockCont :: Int,
  comtSleeps :: Int,
  lastLogPage :: Int,
  conflicts :: Int,
  deviceCont :: Int,
  netReceived :: Int,
  netSent :: Int,
  netServices :: Int,
  other :: Int,
  totSwitchDue :: Int
} deriving (Show)

data Task = Task {
  connections :: Int,
  taskSwitch :: [TaskSwitch],
  taskSwitchDue :: TaskSwitchDue,
  totSwitch :: Int
} deriving (Show)

data Transaction = Transaction {
  commited :: Int,
  inserts :: Int,
  updates :: Int,
  deletes :: Int,  
  flushes :: UlcFlush,
  ulsSemReqs :: Request,
  logSemReqs :: Request,
  avgLogWrites :: Double
} deriving (Show)

data Request = Request {
  granted :: Int,
  waited :: Int,
  totReq :: Int
} deriving (Show)

data UlcFlush = UlcFlush {
  fullUlc :: Int,
  endTran :: Int,
  changeDB :: Int,
  logRecord :: Int,
  byUnpin :: Int,
  byOther :: Int,
  totFlush :: Int
} deriving (Show)

data Index = Index {
  splits :: Int,
  shrinks :: Int    
} deriving (Show)

data Lock = Lock {
  lockReqs :: Int,
  lockCont :: Int,
  deadlocks :: Int,
  exTable :: Request,
  shTable :: Request,  
  exIntent :: Request,
  shIntent :: Request,
  exPage :: Request,
  upPage :: Request,
  shPage :: Request,
  exRow :: Request,
  upRow :: Request,
  shRow :: Request,
  exAddress :: Request,
  shAddress :: Request,
  lpLock :: Request,
  promotions :: Int,
  timeouts :: Int
} deriving (Show)

data Cache = Cache {
  cacheHits :: Int,
  cacheMisses :: Int,
  dirtyBuffers :: Double,
  totCache :: Int,
  caches :: [NamedCache]
} deriving (Show)

data NamedCache = NamedCache {
  cacheName :: String,
  spinContention :: Double,
  utilization :: Double,
  hits :: Int,
  wash :: Int,
  misses :: Int,
  totHitsMiss :: Int,
  largeIO :: Int,
  largeIOTotal :: Int 
} deriving (Show)

data Disk = Disk {
  enginesIO :: [EngineIO],
  delayByDiskIO :: Int,
  delayByServer :: Int,
  delayByEngine :: Int,
  delayByOS :: Int,
  requestedIO :: Int,
  completedIO :: Int,
  devices :: [Device]
} deriving (Show)

data EngineIO = EngineIO {
  engineName :: String,
  outstandIO :: Int
} deriving (Show)

data Device = Device {
  deviceName :: String,
  totalIO :: Int
} deriving (Show)

data Sysmon = Sysmon {
  sysmonTime :: LogInterval,
  kernel :: Kernel,
  task :: Task,
  transaction :: Transaction,
  index :: Index,
  lock :: Lock,
  cache :: Cache,
  disk :: Disk 
} deriving (Show)

$(deriveAverage ''EngineBusy)
$(deriveAverage ''CpuYield)
$(deriveAverage ''Kernel)
$(deriveAverage ''TaskSwitch)
$(deriveAverage ''TaskSwitchDue)
$(deriveAverage ''Task)
$(deriveAverage ''Transaction)
$(deriveAverage ''Request)
$(deriveAverage ''UlcFlush)
$(deriveAverage ''Index)
$(deriveAverage ''Lock)
$(deriveAverage ''Cache)
$(deriveAverage ''NamedCache)
$(deriveAverage ''Disk)
$(deriveAverage ''EngineIO)
$(deriveAverage ''Device)
$(deriveAverage ''Sysmon)