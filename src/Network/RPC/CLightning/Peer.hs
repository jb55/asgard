
module Network.RPC.Peer
    ( Peer(..)
    ) where

import Network.RPC.PeerState (PeerState)
import Data.Text (Text)

data Peer = Peer {
    peerAddress :: Text
  , peerState   :: PeerState
  }

instance FromJSON Peer where
  parseJSON (Object obj) =
    Peer <$> obj .: "netaddr"
         <*> obj .: "state"
