{-# LANGUAGE KindSignatures #-}

module Test.Tornado where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Hedgehog
import Network.RPC.CLightning.Peer (Peer(..))
import Network.RPC.CLightning.Peer (Peer(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.State as Gen
import qualified Hedgehog.Range as Range

newtype Address = Address { address :: ByteString }
    deriving (Show, Eq)

newtype Satoshis = Satoshis { satoshis :: Int }
    deriving (Show, Eq)

data BitcoinMethod = GenerateBlock
                   | SendToAddress Address Satoshis
                   deriving (Show, Eq)

data LightningMethod = NewAddr
                     deriving (Show, Eq)

newtype BitcoinEvent (v :: * -> *) = BitcoinEvent BitcoinMethod
                                   deriving (Eq, Show)

newtype LightningEvent (v :: * -> *) = LightningEvent LightningMethod
                                     deriving (Eq, Show)

data FundWallet (v :: * -> *) =
    FundWallet Satoshis
  deriving (Eq, Show)

instance HTraversable FundWallet where
  htraverse _ (FundWallet sats) =
    pure (FundWallet sats)

instance HTraversable BitcoinEvent where
  htraverse _ (BitcoinEvent event) =
    pure (BitcoinEvent event)

data State v =
  State {
      stateUnspent :: Map Address (Var Satoshis v)
    , statePeers :: Set (Var Peer v)
    } deriving (Eq, Show)


initialState :: State v
initialState =
  State {
    stateUnspent = Map.empty
  , statePeers   = Map.empty
  }

newAddr :: (Monad n, MonadIO m) => Command n m State
newAddr =
  let
    gen state =
      Just (pure (LightningEvent NewAddr))

    execute _ =
      newAddr
  in
    Command gen execute [
        Update $ \s _i o ->
          s {
            stateUnspent =
              Set.insert o (statePids s)
          }
      ]


fundWallet :: (Monad n, MonadIO m) => Command n m State
fundWallet =
  let
    gen _ =
      Just $
        pure Spawn

    execute _ = do
      sendtoaddress
  in
    Command gen execute [
        Update $ \s _i o ->
          s {
            state =
              Set.insert o (statePids s)
          }
      ]
