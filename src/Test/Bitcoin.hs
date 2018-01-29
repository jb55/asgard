{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bitcoin  where


import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (hPutStrLn)

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word16)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), openFile, hClose, Handle)
import System.Posix.Signals (signalProcess)
import System.Posix.Types (CPid)
import System.Process
import System.Process.Internals
import System.Timeout (timeout)
import Text.Printf (printf)
import UnliftIO (MonadUnliftIO)

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified System.Process as P
import qualified UnliftIO.Exception as UIO

import Test.JsonRPC
import Test.Tailable hiding (timeout)


data Bitcoin =
  Bitcoin {
      bitcoinDir  :: FilePath
    , bitcoinPort :: !Word16
    , bitcoinArgs :: [String]
    , bitcoinRPC  :: JsonRPC
  } deriving Show

-- | returns Just pid or Nothing if process has already exited
getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing


data BitcoinProc =
  BitcoinProc {
      bitcoinStdout     :: Handle
    , bitcoinProcess    :: ProcessHandle
    , bitcoinProcessPID :: CPid
    }

instance Show BitcoinProc where
  show BitcoinProc{..} =
      printf "BitcoinProc [%d]" (fromIntegral bitcoinProcessPID :: Int)

initBitcoin :: MonadIO m => FilePath -> Word16 -> m Bitcoin
initBitcoin dir port = liftIO $ do
  let regtestdir  = dir        </> "regtest"
      regConfPath = regtestdir </> "bitcoin.conf"
      confPath    = dir        </> "bitcoin.conf"
      cmdline = [
          "-datadir=" ++ dir
        , "-server"
        , "-printtoconsole"
        , "-regtest"
        --, "-debug"
        , "-logtimestamps"
        ]
  writeConfig regConfPath port
  writeConfig confPath port

  manager <- newManager defaultManagerSettings

  let host      = "localhost"
      user      = "rpcuser"
      pass      = "rpcpass"
      intport   = fromIntegral port
      rpc       = makeClient manager host intport user pass

  return (Bitcoin {
           bitcoinDir  = dir
         , bitcoinPort = port
         , bitcoinRPC  = rpc
         , bitcoinArgs = cmdline
         })


startBitcoin :: (MonadUnliftIO m, MonadLoggerIO m) => Bitcoin -> m BitcoinProc
startBitcoin Bitcoin{..} = do
  let p = (P.proc "bitcoind" bitcoinArgs) { std_out = CreatePipe
                                          , close_fds = False
                                          }

  (_, mstdout, _, procHandle) <- liftIO $ createProcess_ "bitcoind" p
  mpid <- liftIO $ getPid procHandle

  stdout <- maybe (fail "Could not open bitcoind stdout") return mstdout
  pid    <- maybe (fail "Could not grab bitcoind pid") return mpid

  let btcproc = BitcoinProc {
      bitcoinStdout     = stdout
    , bitcoinProcess    = procHandle
    , bitcoinProcessPID = fromIntegral pid
    }

  $(logInfo) ("Starting " <> T.pack (show btcproc))

  return btcproc

bs :: ByteString -> ByteString
bs s = s

stopBitcoin :: MonadLoggerIO m => BitcoinProc -> m ()
stopBitcoin b@BitcoinProc{..} = do
  $(logInfo) ("Terminating " <> T.pack (show b))
  liftIO $ terminateProcess bitcoinProcess
  ma <- liftIO $ timeout (30 * 1000000) (waitForProcess bitcoinProcess)
  maybe timedOut (return . const ()) ma
  liftIO $ hClose bitcoinStdout
  where
    timedOut :: MonadLoggerIO m => m ()
    timedOut = do
      $(logInfo) "Bitcoin process timed out while try to close. Killing."
      liftIO (signalProcess 9 bitcoinProcessPID)



getnewaddress :: JsonRPC -> IO Text
getnewaddress rpc = call rpc "getnewaddress" (mempty :: Array)

generatetoaddress :: JsonRPC -> Int -> Text -> IO [Text]
generatetoaddress rpc nblocks addr =
  call rpc "generatetoaddress" [toJSON nblocks, toJSON addr]

generateBlocks :: JsonRPC -> Int -> IO [Text]
generateBlocks rpc nblocks = do
  addr <- getnewaddress rpc
  generatetoaddress rpc nblocks addr


writeConfig :: FilePath -> Word16 -> IO ()
writeConfig file port = do
  handle <- openFile file WriteMode
  hPutStrLn handle "rpcuser=rpcuser"
  hPutStrLn handle "rpcpassword=rpcpass"
  hPutStrLn handle ("rpcport=" <> B8.pack (show port))
  hClose handle

withBitcoin :: (MonadUnliftIO m, MonadLoggerIO m)
            => ((Bitcoin, BitcoinProc) -> m c) -> m c
withBitcoin cb = UIO.bracket start stop cb
  where
    start = do
      btc   <- initBitcoin "/tmp/bitcointest" 7888
      bproc <- startBitcoin btc
      return (btc, bproc)
    stop (_, bproc) = stopBitcoin bproc

waitForLoaded :: MonadIO m => BitcoinProc -> m ()
waitForLoaded BitcoinProc{..} =
  evalTailable t (waitForLogs [ bs "Done loading" ])
  where
    t = defaultTailable bitcoinStdout

testbtc :: IO ()
testbtc = runStderrLoggingT $ withBitcoin $ \(btc,btcproc) -> do
  let rpc = bitcoinRPC btc
  waitForLoaded btcproc
  addr <- liftIO (generateBlocks rpc 5)
  liftIO (print addr)
