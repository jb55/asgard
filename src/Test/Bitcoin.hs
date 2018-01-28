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
import Data.Word (Word16, Word8)
import Network.HTTP.Client
import System.Directory (createDirectoryIfMissing)
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
import qualified System.Process as P

import qualified UnliftIO.Exception as UIO
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Base64 as B64

import qualified Data.Text as T

data BitcoinRPC =
  BitcoinRPC {
      btcrpcManager  :: Manager
    , btcrpcReq :: Request
    }

instance Show BitcoinRPC where
  show BitcoinRPC{..} = printf "BitcoinRPC [%s]" (show btcrpcReq)

data Bitcoin =
  Bitcoin {
      bitcoinDir  :: FilePath
    , bitcoinPort :: !Word16
    , bitcoinArgs :: [String]
    , bitcoinRPC  :: BitcoinRPC
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
        , "-printtoconsole"
        , "-server"
        , "-regtest"
        --, "-debug"
        , "-logtimestamps"
        ]

  createDirectoryIfMissing True regtestdir
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


startBitcoin :: (MonadLoggerIO m) => Bitcoin -> m BitcoinProc
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


log :: ByteString -> IO ()
log = B8.putStrLn



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



makeClient :: Manager -> ByteString -> Word16 -> ByteString -> ByteString -> BitcoinRPC
makeClient manager host port user pass =
  let
      authStr = B64.encode (user <> ":" <> pass)
      headers = [("Authorization", "Basic " <> authStr)]

      req = defaultRequest {
              host           = host
            , port           = fromIntegral port
            , method         = "POST"
            , requestHeaders = headers
            }

  in BitcoinRPC manager req


data JsonRPC a = JsonRPC {
      jrpcResult :: a
    , jrpcError  :: Maybe Text
    , jrpcId     :: Int
    }
    deriving Show

instance FromJSON a => FromJSON (JsonRPC a) where
    parseJSON (Object obj) =
        JsonRPC <$> obj .:  "result"
                <*> obj .:? "error"
                <*> obj .:  "id"

call :: (ToJSON a, FromJSON b) => BitcoinRPC -> Text -> a -> IO b
call BitcoinRPC{..} method params =
    let
        reqData =
            object [ "jsonrpc" .= T.pack "2.0"
                   , "method"  .= method
                   , "params"  .= params
                   , "id"      .= (1 :: Word8)
                  ]
        req =
            btcrpcReq {
              requestBody = RequestBodyLBS (JSON.encode reqData)
            }
    in
      do
        res <- httpLbs req btcrpcManager
        let body = responseBody res
        case JSON.eitherDecode body of
          Left e -> fail ("Could not decode JSON: \n\n" <> e <> "\n\n" <> show body )
          Right jrpcres  -> return (jrpcResult jrpcres)

getnewaddress :: BitcoinRPC -> IO Text
getnewaddress rpc = call rpc "getnewaddress" (mempty :: Array)

generatetoaddress :: BitcoinRPC -> Int -> Text -> IO [Text]
generatetoaddress rpc nblocks addr =
  call rpc "generatetoaddress" [toJSON nblocks, toJSON addr]

generateBlocks :: BitcoinRPC -> Int -> IO [Text]
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


test :: IO ()
test = runStderrLoggingT $ withBitcoin $ \(btc,_) -> do
  let rpc = bitcoinRPC btc
  addr <- liftIO (generateBlocks rpc 1)
  liftIO (print addr)
