{-# LANGUAGE RecordWildCards #-}

module Test.Proc where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Monoid ((<>))
import System.IO (Handle)
import System.IO (hClose)
import System.Posix.Signals (signalProcess)
import System.Posix.Types (CPid)
import System.Process (terminateProcess, waitForProcess)
import System.Process.Internals
import System.Timeout (timeout)
import Text.Printf (printf)

import qualified Data.Text as T

data Proc =
  Proc {
      procStdout :: Handle
    , procHandle :: ProcessHandle
    , procPID    :: CPid
    }

instance Show Proc where
  show Proc{..} =
      printf "Proc [%d]" (fromIntegral procPID :: Int)

-- | returns Just pid or Nothing if process has already exited
getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

stopProc :: MonadLoggerIO m => Proc -> m ()
stopProc proc@Proc{..} = do
  logInfoN ("Terminating " <> T.pack (show proc))
  liftIO $ terminateProcess procHandle
  ma <- liftIO $ timeout (30 * 1000000) (waitForProcess procHandle)
  maybe timedOut (return . const ()) ma
  liftIO $ hClose procStdout
  where
    timedOut :: MonadLoggerIO m => m ()
    timedOut = do
      logInfoN "Process timed out while try to close. Killing."
      liftIO (signalProcess 9 procPID)
