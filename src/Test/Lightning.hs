{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Word (Word16)
import Network.RPC (rpcRequest, ListPeers(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process
import Text.Regex.TDFA
import UnliftIO (MonadUnliftIO)

import Regex.ToTDFA
import Network.RPC.Config

import Test.Tailable
import Test.Proc

import qualified System.Process as P
import qualified Data.Text as T
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


newtype LightningProc = LightningProc { getLightningProc :: Proc }
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

startLightning :: (MonadUnliftIO m, MonadLoggerIO m) => LightningD -> m LightningProc
startLightning LightningD{..} = do
  let p = (P.proc "lightningd" lightningArgs)
            { std_out = CreatePipe
            , close_fds = False
            }

  (_, mstdout, _, pHandle) <- liftIO $ createProcess_ "lightningd/lightningd" p
  mpid <- liftIO $ getPid pHandle

  stdout <- maybe (fail "Could not open bitcoind stdout") return mstdout
  pid    <- maybe (fail "Could not grab bitcoind pid") return mpid

  let lnproc = Proc {
      procStdout = stdout
    , procHandle = pHandle
    , procPID    = fromIntegral pid
    }

  logInfoN ("Starting " <> T.pack (show lnproc))

  return (LightningProc lnproc)

waitForLoaded :: MonadIO m => LightningProc -> m ()
waitForLoaded (LightningProc Proc{..}) =
  waitForLog procStdout "Hello world from"

withLightning :: (MonadUnliftIO m, MonadLoggerIO m)
              => ((LightningD, LightningProc) -> m c) -> m c
withLightning cb = UIO.bracket start stop cb
  where
    lightningDir = LightningDir "/tmp/lightningtest"
    bitcoinDir   = BitcoinDir "/tmp/bitcointest"
    start = do
      ln     <- initLightning RandomHSM bitcoinDir lightningDir 9785
      lnproc <- startLightning ln
      return (ln, lnproc)
    stop (_, LightningProc p) = stopProc p

testln :: IO ()
testln = runStderrLoggingT $ withLightning $ \(ln,lnproc) -> do
  let rpc = lightningRPC ln
  waitForLoaded lnproc
  resp <- liftIO (rpcRequest rpc ListPeers)
  liftIO (print resp)
