{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.CLightning.Peer
    ( Peer(..)
    ) where

import Data.Aeson
import Network.RPC.PeerState (PeerState)
import Data.Text (Text)

data Peer = Peer {
    peerAddress :: [ Text ]
  , peerState   :: PeerState
  } deriving Show

instance FromJSON Peer where
  parseJSON (Object obj) =
    Peer <$> obj .: "netaddr"
         <*> obj .: "state"
