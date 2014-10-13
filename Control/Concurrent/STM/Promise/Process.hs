{-# LANGUAGE RecordWildCards, CPP, ScopedTypeVariables #-}
-- | Promises for processes
module Control.Concurrent.STM.Promise.Process
    ( processPromise, processPromiseCallback
    , ProcessResult(..), ExitCode(..)) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Promise

import Control.Exception

import System.Process
import System.IO
import System.IO.Error
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
                    silent (hPutStr inh input)
                    silent (hFlush inh)

                silent (hClose inh)

                let go = do
                    ex_code <- waitForProcess pid
                    out <- hGetContents outh
                    err <- hGetContents errh
                    a <- evaluate (length out)
                    b <- evaluate (length err)
                    a `seq` b `seq` do
                        silent (hClose outh)
                        silent (hClose errh)
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

                v <- readTVar result_var
                when (v == Unfinished) (writeTVar result_var Cancelled)

                writeTVar spawn_ok False

                swapTVar pid_var Nothing

            case m_pid of
                Just pid -> void $ forkIO $ silent $ do
                    -- terminateProcess pid
                    terminateProcess9 pid
                    ex_code <- waitForProcess pid
                    ex_code `seq` return ()
                Nothing  -> return ()

        result = readTVar result_var

    return Promise {..}

-- from http://stackoverflow.com/questions/8820903
terminateProcess9 :: ProcessHandle -> IO ()
terminateProcess9 ph = do
#if __GLASGOW_HASKELL__ >= 708
    let ProcessHandle pmvar _ = ph
#else
    let ProcessHandle pmvar = ph
#endif
    posixh <- readMVar pmvar
    case posixh of
        OpenHandle pid -> signalProcess 9 pid
        _ -> return ()

-- | Make a `Promise`
processPromise
    :: FilePath                   -- ^ Program to run
    -> [String]                   -- ^ Arguments
    -> String                     -- ^ Input string (stdin)
    -> IO (Promise ProcessResult) -- ^ Promise object
processPromise = processPromiseCallback (\ _ -> return ())
