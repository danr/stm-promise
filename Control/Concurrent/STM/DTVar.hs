-- | TVars with a dirty bit that can allow for one listener
--
--   I guess one could generalize this with a list or channel of
--   listeners that wake up when the variable is dirty
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

import Control.Monad
import Control.Concurrent.STM

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

newDTVarIO :: a -> IO (DTVar a)
newDTVarIO = atomically . newDTVar

writeDTVar :: DTVar a -> a -> STM ()
writeDTVar (DTVar v d) x = do
    writeTVar v x
    writeTVar d True

writeDTVarIO :: DTVar a -> a -> IO ()
writeDTVarIO u x = atomically (writeDTVar u x)

-- | Reads a DTVar without changing the dirty bit
readDTVar :: DTVar a -> STM a
readDTVar = readTVar . var

-- | Reads a DTVar without changing the dirty bit
readDTVarIO :: DTVar a -> IO a
readDTVarIO = atomically . readDTVar

-- | Listens until the dirty bit is true, then reads and sets dirty to false
listenDTVar :: DTVar a -> STM a
listenDTVar u = do
    d <- readTVar (dirty u)
    unless d retry
    writeTVar (dirty u) False
    readTVar (var u)

listenDTVarIO :: DTVar a -> IO a
listenDTVarIO = atomically . listenDTVar

listenDTVars :: [DTVar a] -> STM [a]
listenDTVars us = do
    ds <- mapM (readTVar . dirty) us
    unless (or ds || null ds) retry
    mapM_ ((`writeTVar` False) . dirty) us
    mapM (readTVar . var) us

listenDTVarsIO :: [DTVar a] -> IO [a]
listenDTVarsIO = atomically . listenDTVars

modifyDTVar :: DTVar a -> (a -> a) -> STM ()
modifyDTVar u f = do
    x <- readTVar (var u)
    writeDTVar u (f x)

