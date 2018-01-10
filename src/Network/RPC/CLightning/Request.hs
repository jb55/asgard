{-# LANGUAGE QuasiQuotes #-}

module Network.RPC.CLightning.Request
    ( makeRequest
    ) where

import Data.Aeson
import Data.Aeson.QQ

makeRequest :: String -> Value
makeRequest req = [aesonQQ| {"method": #{req}, "id":"test", "params":[]}|]
