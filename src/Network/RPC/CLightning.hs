
module Network.RPC.CLightning where

import Control.Monad.IO.Class (MonadIO(..))
import Network.RPC.CLightning.Peer
import Network.RPC

listPeers :: MonadIO m => SocketConfig -> m [Peer]
listPeers cfg = getPeersResp <$> rpc_ cfg "listpeers"
