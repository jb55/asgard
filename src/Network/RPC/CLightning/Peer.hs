{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.RPC.CLightning.Peer
    ( Peer(..)
    , PeerChan(..)
    , ListPeersResp(..)
    , ListPeers(..)
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

data PeerChan = PeerChan {
      peerChanState       :: PeerState
    , peerChanOwner       :: Maybe Text
    , peerChanId          :: Maybe ShortId
    , peerChanMSatsToUs   :: Maybe MSats
    , peerChanMSatsTotal  :: Maybe MSats
    , peerChanFundingTxId :: Maybe Text
    } deriving Show

data Peer = Peer {
    peerAddress     :: Maybe [Text]
  , peerId          :: Text
  , peerConnected   :: Bool
  , peerChannels    :: [PeerChan]
  } deriving Show

instance FromJSON Peer where
  parseJSON (Object obj) =
    Peer
      <$> obj .:? "netaddr"
      <*> obj .:  "id"
      <*> obj .:  "connected"
      <*> obj .:  "channels"

instance FromJSON PeerChan where
  parseJSON (Object obj) =
    PeerChan
      <$> obj .:  "state"
      <*> obj .:? "owner"
      <*> obj .:? "short_channel_id"
      <*> obj .:? "msatoshi_to_us"
      <*> obj .:? "msatoshi_total"
      <*> obj .:? "funding_txid"


newtype ListPeersResp = ListPeersResp { getPeersResp :: [Peer] }
  deriving Show

data ListPeers = ListPeers
  deriving Show

instance ToJSON ListPeers where
  toJSON _ = makeRequest "listpeers"

instance FromJSON ListPeersResp where
  parseJSON (Object obj) =
    ListPeersResp <$> obj .: "peers"

type instance Resp ListPeers = ListPeersResp
