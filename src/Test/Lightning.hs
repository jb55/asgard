{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Lens (IsByteString(packedBytes))
import Data.Word (Word16, Word8)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Numeric.Lens (hex)
import System.FilePath ((</>))
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString (regexec)

import Regex.ToTDFA
import Network.RPC.Config

import Test.JsonRPC
import Test.Proc

import qualified Data.ByteString.Char8 as B8

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

initLightning :: MonadLogger m => HSM -> BitcoinDir -> LightningDir -> Word16 -> m LightningD
initLightning hsm bd@BitcoinDir{..} ld@LightningDir{..} port = do
  let mseed = B8.pack lightningdir =~ bstr "([^/]+)/*$"  :: [[ByteString]]
      intport = fromIntegral port
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

  return $ LightningD {
                lightningDir    = ld
              , lightningBtcDir = bd
              , lightningPort   = port
              , lightningArgs   = cmdline
              , lightningRPC    = rpc
              }

        -- if not os.path.exists(lightning_dir):
        --     os.makedirs(lightning_dir)
