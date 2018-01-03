{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

module Network.RPC.CLightning.Commands
    ( GetPeers(..)
    , GetPeersResp(..)
    , CRPCResp(..)
    ) where

import Network.RPC.Common (Resp)
import Network.RPC.CLightning.Peer (Peer)
import Data.Aeson
import Data.Aeson.QQ

data GetPeers = GetPeers

type instance Resp GetPeers = GetPeersResp

newtype CRPCResp a = CRPCResp { getCRPCResp :: a }
  deriving Show

newtype GetPeersResp = GetPeersResp { getPeersResp :: [Peer] }
  deriving Show

makeRequest :: String -> Value
makeRequest req = [aesonQQ| {"method": #{req}, "id":"test", "params":[]}|]

instance FromJSON a => FromJSON (CRPCResp a) where
  parseJSON (Object obj) =
    CRPCResp <$> obj .: "result"

instance FromJSON GetPeersResp where
  parseJSON (Object obj) =
    GetPeersResp <$> obj .: "peers"

instance ToJSON GetPeers where
  toJSON _ = makeRequest "getpeers"
