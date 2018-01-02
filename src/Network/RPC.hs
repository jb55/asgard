
module Network.RPC
    ( rpcRequest
    ) where


rpcRequest :: (ToJSON a, FromJSON (Resp a)) => RPCConfig -> a -> IO (Resp a)
rpcRequest = undefined


