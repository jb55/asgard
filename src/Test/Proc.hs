{-# LANGUAGE RecordWildCards #-}

module Test.Proc where

import System.IO (Handle)
import System.Posix.Types (CPid)
import System.Process
import System.Process.Internals
import Text.Printf (printf)

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
