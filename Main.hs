{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

import Network.RPC
import Network
import Network.RPC.Common
import Data.Aeson
import Network.RPC.Error

cfg :: RPCConfig
cfg = RPCConfig "/home/jb55/.lightning/lightning-rpc" Nothing

rpc
  :: (ToJSON a, FromJSON (Resp a)) => a -> IO (Either RPCError (Resp a))
rpc = rpcRequest cfg
