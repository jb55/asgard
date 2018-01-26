{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bitcoin (Bitcoin, initBitcoin) where

import Control.Monad (unless)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (hPutStrLn)
import Data.ByteString.Short (ShortByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word16, Word8)
import Network.HTTP.Client
import Network.HTTP.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), openFile, hClose, Handle)
import System.Process (createProcess, ProcessHandle)
import Text.Printf (printf)

import Network.Bitcoin.Api.Wallet
import Network.Bitcoin.Api.Client (Client(..))

import qualified Data.ByteString.Char8 as B8
import qualified System.Process as P
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
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

data BitcoinProc =
  BitcoinProc {
      bitcoinStdout  :: Handle
    , bitcoinProcess :: ProcessHandle
    }

initBitcoin :: FilePath -> Word16 -> IO Bitcoin
initBitcoin dir port = do
  let regtestdir  = dir        </> "regtest"
      regConfPath = regtestdir </> "bitcoin.conf"
      confPath    = dir        </> "bitcoin.conf"
      cmdline = [
          "bitcoind"
        , "-datadir=" ++ dir
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

  let btcConfig = regtestdir </> "bitcoin.conf"
      host      = "localhost"
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

startBitcoin :: Bitcoin -> IO BitcoinProc
startBitcoin Bitcoin{..} = do
  (_, Just stdout, _, procHandle) <- createProcess (P.proc "bitcoind" bitcoinArgs)
  return $
    BitcoinProc {
      bitcoinStdout  = stdout
    , bitcoinProcess = procHandle
    }

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



rpcCall :: (ToJSON a, FromJSON b) => BitcoinRPC -> Text -> a -> IO b
rpcCall BitcoinRPC{..} method params =
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
        case JSON.decode body of
          Nothing -> fail ("Could not decode JSON: \n\n" <> show body)
          Just v  -> return v


writeConfig :: FilePath -> Word16 -> IO ()
writeConfig file port = do
  handle <- openFile file WriteMode
  hPutStrLn handle "rpcuser=rpcuser"
  hPutStrLn handle "rpcpassword=rpcpass"
  hPutStrLn handle ("rpcport=" <> B8.pack (show port))
  hClose handle
