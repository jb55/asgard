{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}

module Test.Proc where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import UnliftIO (MonadUnliftIO)
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
import qualified UnliftIO as UIO
import qualified System.Process as P

data Started
data Stopped

-- external stuff
data Loading
data Loaded

instance Show Started  where show _ = "Started"
instance Show Stopped  where show _ = "Stopped"

instance Show Loading  where show _ = "Loading"
instance Show Loaded   where show _ = "Loaded"

data Proc a =
  Proc {
      procStdout :: Handle
    , procHandle :: ProcessHandle
    , procName   :: String
    , procPID    :: CPid
    }

instance Show (Proc a) where
  show Proc{..} =
      printf "%s [%d]" procName (fromIntegral procPID :: Int)

-- | returns Just pid or Nothing if process has already exited
getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

stopProc :: MonadLoggerIO m => Proc Started -> m (Proc Stopped)
stopProc proc@Proc{..} = do
  logInfoN ("Terminating " <> T.pack (show proc))
  ma <- liftIO $ timeout 100000 (terminateProcess procHandle)
  maybe timedOut (return . const ()) ma
  ma <- liftIO $ timeout 100000 (waitForProcess procHandle)
  maybe timedOut (return . const ()) ma
  logInfoN ("Terminated " <> T.pack (show proc))
  liftIO $ hClose procStdout
  -- coerce return type
  return proc{ procName = procName }
  where
    timeoutLen = 10 * 1000000
    timedOut :: MonadLoggerIO m => m ()
    timedOut = do
      logInfoN "Process timed out while try to close. Killing."
      liftIO (signalProcess 9 procPID)


crashRestartProc :: MonadLoggerIO m => [String] -> Proc Started -> m (Proc Started)
crashRestartProc args p@Proc{..} = do
  logInfoN ("Crashing " <> T.pack (show p))
  liftIO (signalProcess 9 procPID)
  liftIO (waitForProcess procHandle)
  startProc procName args


startProc :: MonadLoggerIO m => String -> [String] -> m (Proc Started)
startProc procname args = do
  let p = (P.proc procname args)
            { std_out = CreatePipe
            , close_fds = False
            }

  (_, mstdout, _, pHandle) <- liftIO (createProcess_ procname p)
  mpid <- liftIO (getPid pHandle)

  stdout <- maybe (fail "Could not open process stdout") return mstdout
  pid    <- maybe (fail "Could not grab process pid") return mpid

  let lnproc = Proc {
      procStdout = stdout
    , procHandle = pHandle
    , procName   = procname
    , procPID    = fromIntegral pid
    }

  logInfoN ("Starting " <> T.pack (show lnproc))

  return lnproc
