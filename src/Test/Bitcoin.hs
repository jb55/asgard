{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bitcoin where


import Control.Concurrent (threadDelay, forkIO)
import Control.Lens
import Control.Monad (replicateM_, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Lens (_Integer, key)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word16)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), openFile, hClose, hGetLine)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, wait)

import qualified Data.ByteString.Char8 as B8
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

  manager <- newManager defaultManagerSettings

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

setupBitcoin :: MonadIO m => JsonRPC -> m ()
setupBitcoin rpc = do
  blocks <- getblocks rpc
  void $ when (blocks < 432) (void (generateBlocks rpc (432 - blocks)))



    -- info = bitcoind.rpc.getblockchaininfo()
    -- # Make sure we have segwit and some funds
    -- if info['blocks'] < 432:
    --     logging.debug("SegWit not active, generating some more blocks")
    --     bitcoind.generate_block(432 - info['blocks'])
    -- elif bitcoind.rpc.getwalletinfo()['balance'] < 1:
    --     logging.debug("Insufficient balance, generating 1 block")
    --     bitcoind.generate_block(1)

getblocks :: MonadIO m => JsonRPC -> m Int
getblocks rpc = do
  res :: Value <- call_ rpc "getblockchaininfo"
  return (res ^?! key "blocks" . _Integer . to fromIntegral)

sendtoaddress :: MonadIO m => JsonRPC -> Text -> Double -> m Text
sendtoaddress rpc addr sats =
  call rpc "sendtoaddress" [toJSON addr, toJSON sats]

getnewaddress :: MonadIO m => JsonRPC -> m Text
getnewaddress rpc = call_ rpc "getnewaddress"

generatetoaddress :: MonadIO m => JsonRPC -> Int -> Text -> m [Text]
generatetoaddress rpc nblocks addr =
  call rpc "generatetoaddress" [toJSON nblocks, toJSON addr]

generateBlocks :: MonadIO m => JsonRPC -> Int -> m [Text]
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
  getAddr <- async (generateBlocks rpc 5)
  waitForLogs stdout (replicate 5 (bstr "AddToWallet"))
  addr <- wait getAddr
  liftIO (print addr)
