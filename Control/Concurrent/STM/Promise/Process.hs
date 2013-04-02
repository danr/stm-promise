{-# LANGUAGE RecordWildCards #-}
-- | Promises for processes
module Control.Concurrent.STM.Promise.Process
    ( processPromise, processPromiseCallback
    , ProcessResult(..), ExitCode(..)) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Promise

import Control.Exception

import System.Process
import System.IO
import System.Exit

import System.Process.Internals
import System.Posix.Signals

-- | The result from a process
data ProcessResult = ProcessResult
    { stderr :: String
    , stdout :: String
    , excode :: ExitCode
    }
  deriving (Eq, Ord, Show)

-- | Make a `Promise`, but add a callback that will
--   be run when the process finishes nicely.
--   This hook is mainly intended for logging.
processPromiseCallback
    :: (ProcessResult -> IO ())   -- ^ Callback
    -> FilePath                   -- ^ Program to run
    -> [String]                   -- ^ Arguments
    -> String                     -- ^ Input string (stdin)
    -> IO (Promise ProcessResult) -- ^ Promise object
processPromiseCallback callback cmd args input = do

    pid_var    <- newTVarIO Nothing
    result_var <- newTVarIO Unfinished
    spawn_ok   <- newTVarIO True

    let silent io = io `catchError` const (return ())

        spawn = do

            -- Check that the process hasn't been spawned before
            spawn_now <- atomically $ swapTVar spawn_ok False

            when spawn_now $ do

                (Just inh, Just outh, Just errh, pid) <- createProcess $
                    (proc cmd args)
                         { std_in  = CreatePipe
                         , std_out = CreatePipe
                         , std_err = CreatePipe
                         }

                atomically $ writeTVar pid_var (Just pid)

                unless (null input) $ do
                    hPutStr inh input
                    hFlush inh

                hClose inh

                let go = do
                    ex_code <- waitForProcess pid
                    out <- hGetContents outh
                    err <- hGetContents errh
                    a <- evaluate (length out)
                    b <- evaluate (length err)
                    a `seq` b `seq` do
                        hClose outh
                        hClose errh
                        let res = ProcessResult
                                { stderr = err
                                , stdout = out
                                , excode = ex_code
                                }
                        atomically $ writeTVar result_var (An res)
                        callback res

                go `catchError` \ _ -> atomically (writeTVar result_var Cancelled)

        cancel = do

            m_pid <- atomically $ do

                writeTVar result_var Cancelled

                writeTVar spawn_ok False

                swapTVar pid_var Nothing

            case m_pid of
                Just pid -> silent $ do
                    withProcessHandle_ pid $ \ mph -> case mph of
                        OpenHandle ph -> signalProcess killProcess ph >> return mph
                        ClosedHandle{} -> return mph
                    ex_code <- waitForProcess pid
                    ex_code `seq` return ()
                Nothing  -> return ()

        result = readTVar result_var

    return Promise {..}


-- | Make a `Promise`
processPromise
    :: FilePath                   -- ^ Program to run
    -> [String]                   -- ^ Arguments
    -> String                     -- ^ Input string (stdin)
    -> IO (Promise ProcessResult) -- ^ Promise object
processPromise = processPromiseCallback (\ _ -> return ())
