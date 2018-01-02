{-# LANGUAGE EmptyDataDecls #-}

module Network.RPC.CLightning.Commands
    ( GetPeers
    ) where

import Network.RPC.Common (Resp)

data GetPeers

type instance Resp GetPeers = [Peer]
