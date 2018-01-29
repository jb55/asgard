{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tailable where

import Control.Monad (unless, foldM_)
import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT(..), MonadState(..))
import Data.ByteString (hGetLine, ByteString)
import System.IO hiding (hGetLine)
import Text.Regex.Base.RegexLike (matchTest, makeRegex)
import Text.Regex.TDFA.ByteString (Regex)
import UnliftIO (MonadUnliftIO(..))


import qualified UnliftIO.Exception as UIO
import qualified UnliftIO.Timeout as UIO
import qualified System.Timeout as Sys
import qualified Data.ByteString.Char8 as B8

data Tailable =
  Tailable {
      tailHandle  :: Handle
    , tailLastPos :: Integer
    , tailTimeout :: Int
    }
    deriving Show

data TailError = TailError TailErrorType (Maybe ByteString)
               deriving Show

data TailErrorType = MatchError
                   | Timeout
                   deriving Show

type TailResult a = Either TailError a

newtype TailableM m a = TailableM { runTailable_ :: StateT Tailable m a }
    deriving (Functor, Applicative, Monad, MonadState Tailable, MonadIO)

class ToTDFA a where
    toTDFA :: a -> Regex

instance ToTDFA Regex where
    toTDFA r = r

instance ToTDFA ByteString where
    toTDFA bs = makeRegex bs

runTailable :: Tailable -> TailableM m a -> m (a, Tailable)
runTailable t = flip runStateT t . runTailable_

defaultTailable :: Handle -> Tailable
defaultTailable h =
  Tailable {
    tailHandle = h
  , tailLastPos = 0
  , tailTimeout = 5000000
  }

untilM_ :: (Monad m) => m Bool -> m ()
untilM_ act = do
  b <- act
  unless b (untilM_ act)

foldM1_ :: (Foldable t, Monad m) => (a -> m ()) -> t a -> m ()
foldM1_ folder xs = foldM_ (\() x -> folder x) () xs

-- keep reading lines from a handle until it matches all regexes in order
waitForLogsH :: MonadIO m => Handle -> [Regex] -> m ()
waitForLogsH handle rs = liftIO $ foldM1_ folder rs
  where
    readline     = hGetLine handle
    folder regex = untilM_ (matchTest regex <$> readline)

waitForLogs :: (MonadIO m, ToTDFA b) => [b] -> TailableM m (TailResult ())
waitForLogs rs = do
  t@Tailable{..} <- get
  isSeekable <- liftIO $ hIsSeekable tailHandle
  when isSeekable $
    liftIO $ hSeek tailHandle AbsoluteSeek tailLastPos
  res <- liftIO $ timeout tailTimeout  $ wait tailHandle
  when isSeekable $ do
    pos <- liftIO $ hTell tailHandle
    put (t { tailLastPos = pos })
  return res
  where
    regexes = map toTDFA rs
    wait h = catching noMatchError (waitForLogsH h regexes)

test :: IO ()
test = do
  writeFile "/tmp/haskelltest" "HI\nderp OPENINGD hey\nCHANNELD_NORMAL"
  h <- openFile "/tmp/haskelltest" ReadMode
  let tailable = defaultTailable h
      rs       = map toTDFA [".*OPENINGD.*", ".*CHANNELD_NORMAL.*" :: ByteString]
  runTailable tailable (waitForLogs rs)
  hClose h


timeout :: MonadUnliftIO m => Int -> m (TailResult a) -> m (TailResult a)
timeout tout io = do
  mres <- UIO.timeout tout io
  return (fromMaybe (Left timeoutError) mres)

catching :: (MonadUnliftIO m, Monad m) => (ByteString -> a) -> m b -> m (Either a b)
catching err io = UIO.catch newIO handler
  where
    newIO = do
      res <- io
      return (Right res)

    handler (e :: SomeException) =
      return (Left $ err (B8.pack (show e)))

tailErr :: TailErrorType -> Maybe ByteString -> TailError
tailErr err m = TailError err m

tailErrStr :: TailErrorType -> ByteString -> TailError
tailErrStr err s = TailError err (Just s)

timeoutError :: TailError
timeoutError = tailErr Timeout Nothing

noMatchError :: ByteString -> TailError
noMatchError = tailErrStr MatchError
