-- | Evaluating promises in parallel
module Control.Concurrent.STM.Promise.Workers (workers,worker,evaluatePromise) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Promise
import Control.Monad

maybeIO :: Maybe a -> (a -> IO b) -> IO (Maybe b)
maybeIO m f = maybe (return Nothing) (fmap Just . f) m

-- | Evaluates a single promise (in the calling thread),
--   maybe using a timeout in microseconds.
evaluatePromise :: Maybe Int -> Promise a -> IO ()
evaluatePromise m_t promise = do

    _m_thr <- maybeIO m_t $ \ timeout -> forkIO $ do
        threadDelay timeout
        cancel promise

    _s_thr <- forkIO $ spawn promise

    atomically $ do
        status <- result promise
        when (isUnfinished status) retry

-- | Evaluates a channel of promises, maybe using a timeout in microseconds.
--   Stops when the channel is empty.
worker :: Maybe Int -> TChan (Promise a) -> IO ()
worker m_t ch = go where
    go = do
        -- putStr "t"
        m_promise <- atomically $ tryReadTChan ch
        -- putStr "T"
        case m_promise of
            Just promise -> evaluatePromise m_t promise >> go
            Nothing -> return ()


-- | Evaluate these promises on n processors, maybe using a timeout in microseconds.
workers :: Maybe Int -> Int -> [Promise a] -> IO ()
workers m_t n xs = do
    ch <- newTChanIO
    atomically $ mapM_ (writeTChan ch) xs
    replicateM_ n $ forkIO $ worker m_t ch

