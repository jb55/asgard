
module Network.RPC.Config
    ( RPCConfig(..)
    ) where

data RPCConfig =
  RPCConfig {
    rpcPath    :: FilePath
  , rpcTimeout :: Maybe Int
  }
  deriving (Show)
