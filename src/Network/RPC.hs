{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Network.RPC
    ( rpcRequest
    , module X
    ) where

import Data.Aeson
import Network.RPC.Common (Resp)
import Data.ByteString.Lazy (toStrict)
import Network.RPC.Config (RPCConfig(..))
import Network.RPC.Internal (sockRequest)

import Network.RPC.CLightning.Commands (GetPeers(..))
import Network.RPC.CLightning.Commands as X

rpcRequest :: (Show (Resp a), ToJSON a, FromJSON (Resp a))
           => RPCConfig -> a -> IO (Maybe (Resp a))
rpcRequest cfg json_ = do
  mres <- sockRequest cfg (toStrict (encode json_))
  let res = fmap getCRPCResp (decode =<< mres)
  return res


