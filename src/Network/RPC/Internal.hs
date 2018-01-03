{-# LANGUAGE RecordWildCards #-}

module Network.RPC.Internal
    ( sockRequest
    ) where

import Data.ByteString
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Network.RPC.Config (RPCConfig(..))
import Network.Socket.ByteString
import Network.RPC.Common (defaultTimeout)
import Network.Socket (socket, Family(AF_UNIX), SocketType(Stream), connect,
                       SockAddr(SockAddrUnix))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy

sockRequest :: RPCConfig -> ByteString -> IO (Maybe Lazy.ByteString)
sockRequest RPCConfig{..} bs = timeout tout $ do
  soc <- socket AF_UNIX Stream 0
  connect soc (SockAddrUnix rpcPath)
  sendAll soc bs
  readAll soc
  where
    tout        = fromMaybe defaultTimeout rpcTimeout
    readAll soc = fmap (Lazy.fromChunks . Prelude.reverse) (readChunks soc [])
    readChunks soc chunks =
      do chunk <- recv soc 4096
         let cs = chunk:chunks
         if BS.length chunk < 4096
           then return cs
           else readChunks soc cs
