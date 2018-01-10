{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.RPC.CLightning.Peer
    ( Peer(..)
    , GetPeersResp(..)
    , GetPeers(..)
    , ShortId(..)
    ) where

import Data.Aeson
import Data.String (IsString)
import Data.ByteString (ByteString)
import Bitcoin.Denomination (MSats)
import Network.RPC.PeerState (PeerState)
import Network.RPC.Common
import Network.RPC.CLightning.Request (makeRequest)
import Data.Text (Text)

data Peer = Peer {
    peerAddress    :: [Text]
  , peerState      :: PeerState
  , peerConnected  :: Bool
  , peerChanShortId :: ShortId
  , peerMSatsToUs  :: MSats
  , peerMSatsTotal :: MSats
  } deriving Show

instance FromJSON Peer where
  parseJSON (Object obj) =
    Peer <$> obj .: "netaddr"
         <*> obj .: "state"
         <*> obj .: "connected"
         <*> obj .: "channel"
         <*> obj .: "msatoshi_to_us"
         <*> obj .: "msatoshi_total"

newtype GetPeersResp = GetPeersResp { getPeersResp :: [Peer] }
  deriving Show

data GetPeers = GetPeers
  deriving Show

instance ToJSON GetPeers where
  toJSON _ = makeRequest "getpeers"

instance FromJSON GetPeersResp where
  parseJSON (Object obj) =
    GetPeersResp <$> obj .: "peers"

type instance Resp GetPeers = GetPeersResp
