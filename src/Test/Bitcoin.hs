{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bitcoin where


import Control.Concurrent (threadDelay, forkIO)
import Control.Lens
import Control.Monad (replicateM_, void, when, replicateM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Lens (_Integer, key)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word16)
import Network.HTTP.Client (newManager, defaultManagerSettings, ManagerSettings(..),
                            responseTimeoutDefault)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), openFile, hClose, hGetLine)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, wait)

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified UnliftIO.Exception as UIO

import Regex.ToTDFA
import Network.RPC.JsonRPC
import Test.Proc
import Test.Tailable hiding (timeout)

newtype BitcoinProc a b = BitcoinProc { bitcoinproc :: Proc b }
    deriving Show

data BitcoinD =
  BitcoinD {
      bitcoinDir  :: FilePath
    , bitcoinPort :: !Word16
    , bitcoinArgs :: [String]
    , bitcoinRPC  :: JsonRPC
  } deriving Show

initBitcoin :: MonadIO m => FilePath -> Word16 -> m BitcoinD
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


  liftIO (createDirectoryIfMissing True regtestdir)
  writeConfig regConfPath port
  writeConfig confPath port

  manager <- newManager (defaultManagerSettings {
                           managerResponseTimeout = responseTimeoutDefault
                        })

  let host      = "localhost"
      user      = "rpcuser"
      pass      = "rpcpass"
      intport   = fromIntegral port
      rpc       = makeClient manager host intport user pass

  return BitcoinD {
           bitcoinDir  = dir
         , bitcoinPort = port
         , bitcoinRPC  = rpc
         , bitcoinArgs = cmdline
         }


startBitcoin :: MonadLoggerIO m => BitcoinD -> m (BitcoinProc Loading Started)
startBitcoin BitcoinD{..} = fmap BitcoinProc (startProc "bitcoind" bitcoinArgs)

setupBitcoin :: MonadLoggerIO m => JsonRPC -> m [Text]
setupBitcoin rpc = do
  blocks <- getblocks rpc
  logDebugN ("setupBitcoin: currently have " <> T.pack (show blocks) <> " blocks")
  address <- getnewaddress rpc "bech32"
  let n = 6
      gen = generateBlocks rpc address n
  -- this is a hack because it seems to blow up past 150 blocks!?
  if blocks < n then gen else return []



    -- info = bitcoind.rpc.getblockchaininfo()
    -- # Make sure we have segwit and some funds
    -- if info['blocks'] < 432:
    --     logging.debug("SegWit not active, generating some more blocks")
    --     bitcoind.generate_block(432 - info['blocks'])
    -- elif bitcoind.rpc.getwalletinfo()['balance'] < 1:
    --     logging.debug("Insufficient balance, generating 1 block")
    --     bitcoind.generate_block(1)

getblocks :: MonadLoggerIO m => JsonRPC -> m Int
getblocks rpc = do
  res :: Value <- call_ rpc "getblockchaininfo"
  liftIO (print res)
  return (res ^?! key "blocks" . _Integer . to fromIntegral)

sendtoaddress :: MonadLoggerIO m => JsonRPC -> Text -> Double -> m Text
sendtoaddress rpc addr sats =
  call rpc "sendtoaddress" [toJSON addr, toJSON sats]

getnewaddress :: MonadLoggerIO m => JsonRPC -> Text -> m Text
getnewaddress rpc addrtype = call rpc "getnewaddress" [addrtype]

generatetoaddress :: MonadLoggerIO m => JsonRPC -> Int -> Text -> m [Text]
generatetoaddress rpc nblocks addr =
  call rpc "generatetoaddress" [toJSON nblocks, toJSON addr]

generateBlocks :: MonadLoggerIO m => JsonRPC -> Text -> Int -> m [Text]
generateBlocks rpc addr nblocks = generatetoaddress rpc nblocks addr


writeConfig :: FilePath -> Word16 -> IO ()
writeConfig file port = do
  handle <- openFile file WriteMode
  hPutStrLn handle "rpcuser=rpcuser"
  hPutStrLn handle "rpcpassword=rpcpass"
  hPutStrLn handle ("rpcport=" <> B8.pack (show port))
  hClose handle

withBitcoin :: (MonadUnliftIO m, MonadLoggerIO m)
            => ((BitcoinD, BitcoinProc Loading Started) -> m c) -> m c
withBitcoin cb = UIO.bracket start stop cb
  where
    start = do
      btc   <- initBitcoin "/tmp/bitcointest" 7888
      bproc <- startBitcoin btc
      return (btc, bproc)
    stop (_, BitcoinProc p) = stopProc p

waitForLoaded :: MonadIO m
              => BitcoinProc Loading Started -> m (BitcoinProc Loaded Started)
waitForLoaded btcproc@(BitcoinProc p@Proc{..}) = do
  waitForLog procStdout "Done loading"
  return btcproc{ bitcoinproc = p }

testbtc :: IO ()
testbtc = liftIO $ runStderrLoggingT $ withBitcoin $ \(btc, BitcoinProc proc_) -> do
  let rpc    = bitcoinRPC btc
      stdout = procStdout proc_
  waitForLog stdout "Done loading"
  addr <- getnewaddress rpc "bech32"
  getAddr <- async (generateBlocks rpc addr 5)
  waitForLogs stdout (replicate 5 (bstr "AddToWallet"))
  addr <- wait getAddr
  liftIO (print addr)
