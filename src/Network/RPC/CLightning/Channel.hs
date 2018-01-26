{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-} 

module Network.RPC.CLightning.Channel
    ( Channel(..)
    , ListChannels(..)
    , ListChannelsResp(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import Network.RPC.Common (Resp)
import Network.RPC.CLightning.Request (makeRequest)

data Channel = Channel {
    channelSource              :: Text
  , channelDestination         :: Text
  , channelShortId             :: Text
  , channelFlags               :: Int
  , channelActive              :: Bool
  , channelLastUpdate          :: Maybe Int
  , channelBaseFeeMillisatoshi :: Maybe Int
  , channelFeePerMillionth     :: Maybe Int
  , channelDelay               :: Maybe Int
  } deriving Show

instance FromJSON Channel where
  parseJSON (Object obj) =
    Channel <$> obj .: "source"
            <*> obj .: "destination"
            <*> obj .: "short_id"
            <*> obj .: "flags"
            <*> obj .: "active"
            <*> obj .:? "last_update"
            <*> obj .:? "base_fee_millisatoshi"
            <*> obj .:? "fee_per_millionth"
            <*> obj .:? "delay"

instance FromJSON ListChannelsResp where
  parseJSON (Object obj) =
    ListChannelsResp <$> obj .: "channels"

newtype ListChannelsResp = ListChannelsResp { getChannelsResp :: [Channel] }
  deriving Show

data ListChannels = ListChannels
  deriving Show

type instance Resp ListChannels = ListChannelsResp

instance ToJSON ListChannels where
  toJSON _ = makeRequest "getchannels"
