-- | TVars with a dirty bit that allows for one listener
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Concurrent.STM.DTVar
    ( DTVar
    , newDTVar
    , newDTVarIO
    , writeDTVar
    , writeDTVarIO
    , readDTVar
    , readDTVarIO
    , listenDTVar
    , listenDTVarIO
    , listenDTVars
    , listenDTVarsIO
    , modifyDTVar
    ) where

--   I guess one could generalize this with a list or channel of
--   listeners that wake up when the variable is dirty

import Control.Monad
import Control.Concurrent.STM

-- | TVars with a dirty bit which allows for one listener.
data DTVar a = DTVar
    { var   :: TVar a
    , dirty :: TVar Bool
    }

-- | New DTVar which starts dirty
newDTVar :: a -> STM (DTVar a)
newDTVar x = do
    a_var <- newTVar x
    t_var <- newTVar True
    return (DTVar a_var t_var)

-- | `newDTVar` in `IO`
newDTVarIO :: a -> IO (DTVar a)
newDTVarIO = atomically . newDTVar

-- | Write a value to a DTVar, making it dirty
writeDTVar :: DTVar a -> a -> STM ()
writeDTVar (DTVar v d) x = do
    writeTVar v x
    writeTVar d True

-- | `writeDTVar` in `IO`
writeDTVarIO :: DTVar a -> a -> IO ()
writeDTVarIO u x = atomically (writeDTVar u x)

-- | Reads a DTVar /without/ changing the dirty bit
readDTVar :: DTVar a -> STM a
readDTVar = readTVar . var

-- | `readDTVar` in `IO`
readDTVarIO :: DTVar a -> IO a
readDTVarIO = atomically . readDTVar

-- | Listens until the dirty bit is true, then removes the dirty bit and
--   returns the read element
listenDTVar :: DTVar a -> STM a
listenDTVar u = do
    d <- readTVar (dirty u)
    unless d retry
    writeTVar (dirty u) False
    readTVar (var u)

-- | `listenDTVar` in `IO`
listenDTVarIO :: DTVar a -> IO a
listenDTVarIO = atomically . listenDTVar

-- | Listen until any of the dirty bits are true, then removes all dirty bits
--   and returns all `DTVar`'s values, in order.
listenDTVars :: [DTVar a] -> STM [a]
listenDTVars us = do
    ds <- mapM (readTVar . dirty) us
    unless (or ds || null ds) retry
    mapM_ ((`writeTVar` False) . dirty) us
    mapM (readTVar . var) us

-- | `listenDTVars` in `IO`
listenDTVarsIO :: [DTVar a] -> IO [a]
listenDTVarsIO = atomically . listenDTVars

-- | Modify a DTVar, making it dirty.
modifyDTVar :: DTVar a -> (a -> a) -> STM ()
modifyDTVar d f = do
    x <- readDTVar d
    writeDTVar d (f x)

