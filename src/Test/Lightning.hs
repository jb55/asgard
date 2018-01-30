{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Lens
import Control.Monad (replicateM_, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Network.RPC (rpc, rpc_)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hGetLine)
import Text.Regex.TDFA
import UnliftIO (MonadUnliftIO)

import Network.RPC.CLightning
import Network.RPC.Config
import Regex.ToTDFA
import Test.Bitcoin (BitcoinD(..), BitcoinProc(..), withBitcoin)
import Test.Proc
import Test.Tailable

import qualified Test.Bitcoin as BTC
import qualified Data.ByteString.Char8 as B8
import qualified UnliftIO as UIO

newtype BitcoinDir = BitcoinDir { bitcoindir :: FilePath }
    deriving Show

newtype LightningDir = LightningDir { lightningdir :: FilePath }
    deriving Show

data LightningD =
  LightningD {
      lightningBtcDir :: BitcoinDir
    , lightningDir    :: LightningDir
    , lightningPort   :: !Word16
    , lightningArgs   :: [String]
    , lightningRPC    :: SocketConfig
  } deriving Show


newtype LightningProc a b = LightningProc { lightningproc :: Proc b }
    deriving Show

newtype Seed = Seed { hsmseed :: ByteString }
    deriving Show

data HSM = RandomHSM
         | DeterministicHSM (Maybe Seed)
         deriving Show

initLightning :: (MonadLogger m, MonadIO m)
              => HSM -> BitcoinDir -> LightningDir -> Word16 -> m LightningD
initLightning hsm bd@BitcoinDir{..} ld@LightningDir{..} port = do
  let mseed = B8.pack lightningdir =~ bstr "([^/]+)/*$"  :: [[ByteString]]
      -- intport = fromIntegral port
      -- TODO: parameterize
      cmdline = [
          "--bitcoin-datadir=" ++ bitcoindir
        , "--lightning-dir=" ++ lightningdir
        , "--port=" ++ show port
        , "--allow-deprecated-apis=false"
        , "--bitcoind-poll=1s"
        , "--cltv-delta=6"
        , "--cltv-final=5"
        -- NOTE: this should only be set when DEVELOPER=1
        , "--dev-broadcast-interval=1000"
        , "--ignore-fee-limits=false"
        , "--locktime-blocks=5"
        , "--log-level=debug"
        , "--network=regtest"
        , "--override-fee-rates=15000/7500/1000"
        ]
      rpc = SocketConfig {
              rpcPath    = lightningdir </> "lightning-rpc"
            , rpcTimeout = Just (10 * 1000000)
            }

  dirseed <- maybe (logWarnN "no seed found in lightning dir" >> return Nothing)
                   (return . Just . Seed)
                   (mseed ^? ix 0 . ix 1)

  let cmds = cmdline ++
               case hsm of
                 RandomHSM -> []
                 DeterministicHSM mseed1 ->
                   case mseed1 <|> dirseed of
                     Nothing -> fail "no seed available for DeterministicHSM"
                     Just Seed{..} ->
                       ["--dev-hsm-seed=" ++ B8.unpack hsmseed]

  liftIO $ createDirectoryIfMissing True lightningdir

  return LightningD {
             lightningDir    = ld
           , lightningBtcDir = bd
           , lightningPort   = port
           , lightningArgs   = cmds
           , lightningRPC    = rpc
           }


startLightning :: (MonadUnliftIO m, MonadLoggerIO m)
               => LightningD
               -> m (LightningProc Loading Started)
startLightning LightningD{..} =
  fmap LightningProc (startProc "lightningd" lightningArgs)



waitForLoaded :: MonadIO m
              => LightningProc Loading Started -> m (LightningProc Loaded Started)
waitForLoaded lp@(LightningProc p@Proc{..}) = do
  waitForLog procStdout "Hello world from"
  return lp{ lightningproc = p }



withLightning :: (MonadUnliftIO m, MonadLoggerIO m)
              => BitcoinProc Loaded Started
              -> ((LightningD, LightningProc Loading Started) -> m c) -> m c
withLightning _ cb = UIO.bracket start stop cb
  where
    lightningDir = LightningDir "/tmp/lightningtest"
    bitcoinDir   = BitcoinDir "/tmp/bitcointest"
    start = do
      ln     <- initLightning RandomHSM bitcoinDir lightningDir 9785
      lnproc <- startLightning ln
      return (ln, lnproc)
    stop (_, LightningProc p) = stopProc p

rmrf :: FilePath -> IO ()
rmrf p = void (try (removePathForcibly p) :: IO (Either SomeException ()))

testln :: IO ()
testln = do
  rmrf "/tmp/lightningtest"
  threadDelay 1000

  runStderrLoggingT $
    withBitcoin $ \(BitcoinD{..}, btcproc@(BitcoinProc bproc)) -> do
      btcproc' <- BTC.waitForLoaded btcproc
      BTC.setupBitcoin bitcoinRPC
      withLightning btcproc' $ \(LightningD{..},lnproc@LightningProc{..}) -> do
        let rpc       = lightningRPC
            btcrpc    = bitcoinRPC
            btcstdout = procStdout bproc
            lnstdout  = procStdout lightningproc
        _    <- waitForLoaded lnproc
        peers <- listPeers rpc
        addr <- newAddr rpc "bech32"
        BTC.sendtoaddress btcrpc addr 1
        moreblocks <- liftIO $ async (BTC.generateBlocks btcrpc 1)
        waitForLog lnstdout "Owning output"
        liftIO (wait moreblocks)
        funds <- listFunds rpc
        -- crashRestartProc lightningArgs lightningproc
        -- liftIO (threadDelay (10 * 1000000))
        liftIO (mapM_ print funds)
        liftIO (print (peers, addr))
