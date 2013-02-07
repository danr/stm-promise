{-# LANGUAGE RecordWildCards #-}
-- | Promises for processes
module Control.Concurrent.STM.Promise.Process
    (processPromise, ProcessResult(..), ExitCode(..)) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Promise

import Control.Concurrent
import Control.Exception

import System.Process
import System.IO
import System.Exit

-- | The result from a process
data ProcessResult = ProcessResult
    { stderr :: String
    , stdout :: String
    , excode :: ExitCode
    }
  deriving (Eq, Ord, Show)

-- | Make a `Promise`
processPromise
    :: FilePath                   -- ^ Program to run
    -> [String]                   -- ^ Arguments
    -> String                     -- ^ Input string (stdin)
    -> IO (Promise ProcessResult) -- ^ Promise object
processPromise cmd args input = do

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

                unless (null input) $ do
                    hPutStr inh input
                    hFlush inh

                hClose inh

                atomically $ writeTVar pid_var (Just pid)

                void $ forkIO $ silent $ do
                    ex_code <- waitForProcess pid
                    out <- hGetContents outh
                    err <- hGetContents errh
                    atomically $ writeTVar result_var $ An ProcessResult
                        { stderr = err
                        , stdout = out
                        , excode = ex_code
                        }
                    a <- evaluate (length out)
                    b <- evaluate (length err)
                    a `seq` b `seq` do
                        hClose outh
                        hClose errh

        cancel = do

            m_pid <- atomically $ do

                writeTVar result_var Cancelled

                writeTVar spawn_ok False

                swapTVar pid_var Nothing

            case m_pid of
                Just pid -> do
                    terminateProcess pid
                    silent $ void $ waitForProcess pid
                Nothing  -> return ()

        result = readTVar result_var

    return Promise {..}


