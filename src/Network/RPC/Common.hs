{-# LANGUAGE TypeFamilies #-}

module Network.RPC.Common
    ( Resp
    , defaultTimeout
    ) where

defaultTimeout :: Int
defaultTimeout = 5*1000000

type family Resp a :: *
