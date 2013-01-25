{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.STM.Promise.Process (processPromise, ProcessResult(..)) where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Promise

import Control.Concurrent
import Control.Exception

import System.Process
import System.IO
import System.Exit

data ProcessResult = ProcessResult
    { stderr :: String
    , stdout :: String
    , excode :: ExitCode
    }
  deriving (Eq, Ord, Show)

processPromise :: FilePath -> [String] -> String -> IO (Promise ProcessResult)
processPromise cmd args input = do

    pid_var    <- newTVarIO Nothing
    thread_var <- newTVarIO Nothing
    result_var <- newTVarIO Unfinished
    spawned    <- newTVarIO False

    let spawn = do

            -- Check that the process hasn't been spawned before
            spawn_now <- atomically $ do
                old <- readTVar spawned
                writeTVar spawned True
                return old

            when spawn_now $ do

                (Just inh, Just outh, Just errh, pid) <- createProcess $
                    (proc cmd args)
                         { std_in  = CreatePipe
                         , std_out = CreatePipe
                         , std_err = CreatePipe
                         }

                atomically $ writeTVar pid_var (Just pid)

                output  <- hGetContents outh

                err <- hGetContents errh

                unless (null input) $ do
                    hPutStr inh input
                    hFlush inh

                hClose inh

                t_id <- forkIO $ do
                    void $ evaluate (length output)
                    hClose outh
                    void $ evaluate (length err)
                    hClose errh
                    ex <- waitForProcess pid
                    atomically $ writeTVar result_var $ An $ ProcessResult
                        { stderr = err
                        , stdout = output
                        , excode = ex
                        }

                atomically $ writeTVar thread_var (Just t_id)

        cancel = do
            atomically (readTVar thread_var) >>= maybe (return ()) killThread
            atomically (readTVar pid_var) >>= maybe (return ()) (\ pid -> do
                    terminateProcess pid
                    void (waitForProcess pid)
                )
            atomically $ writeTVar result_var $ Cancelled

        result = readTVar result_var

    return Promise {..}


