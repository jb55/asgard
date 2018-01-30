{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bitcoin where


import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Control.Monad (replicateM_, void)
import Control.Concurrent (threadDelay, forkIO)
import Data.Aeson
import Data.ByteString.Char8 (hPutStrLn)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word16)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), openFile, hClose, hGetLine)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, wait)

import qualified Data.ByteString.Char8 as B8
import qualified UnliftIO.Exception as UIO

import Regex.ToTDFA
import Test.JsonRPC
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


startBitcoin :: (MonadUnliftIO m, MonadLoggerIO m)
             => BitcoinD -> m (BitcoinProc Loading Started)
startBitcoin BitcoinD{..} = fmap BitcoinProc (startProc "bitcoind" bitcoinArgs)


getnewaddress :: MonadIO m => JsonRPC -> m Text
getnewaddress rpc = call rpc "getnewaddress" (mempty :: Array)

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
