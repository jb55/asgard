{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tailable where

import Control.Exception (SomeException)
import Control.Monad (unless, foldM_)
import Control.Monad (when)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT(..), MonadState(..), evalStateT)
import Data.ByteString (hGetLine, ByteString)
import Data.Maybe (fromMaybe)
import System.IO hiding (hGetLine)
import Text.Regex.Base.RegexLike (matchTest)
import Text.Regex.TDFA.ByteString (Regex)
import Text.Regex.TDFA (makeRegex)
import UnliftIO (MonadUnliftIO(..))

import qualified UnliftIO.Exception as UIO
import qualified UnliftIO.Timeout as UIO

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

runTailable :: Tailable -> TailableM m a -> m (a, Tailable)
runTailable t = flip runStateT t . runTailable_

evalTailable :: Monad m => Tailable -> TailableM m (TailResult a) -> m a
evalTailable t = tailResultThrow . flip evalStateT t . runTailable_
  where
    tailResultThrow m = do
      eres <- m
      either (fail . show) return eres

defaultTailable :: Handle -> Tailable
defaultTailable h =
  Tailable {
    tailHandle = h
  , tailLastPos = 0
  , tailTimeout = 10 * 1000000
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

waitForLogsM :: MonadIO m => [ByteString] -> TailableM m (TailResult ())
waitForLogsM rs = do
  t@Tailable{..} <- get
  isSeekable <- liftIO $ hIsSeekable tailHandle
  when isSeekable $
    liftIO $ hSeek tailHandle AbsoluteSeek tailLastPos
  res <- liftIO $ timeout tailTimeout timeoutMsg  $ wait tailHandle
  when isSeekable $ do
    pos <- liftIO $ hTell tailHandle
    put (t { tailLastPos = pos })
  return res
  where
    timeoutMsg = "Timeout waiting for: " <> B8.pack (show rs)
    regexes = map makeRegex rs
    wait h = catching noMatchError (waitForLogsH h regexes)

test :: IO ()
test = do
  writeFile "/tmp/haskelltest" "HI\nderp OPENINGD hey\nCHANNELD_NORMAL"
  h <- openFile "/tmp/haskelltest" ReadMode
  let tailable = defaultTailable h
  _ <- runTailable tailable (waitForLogsM [".*OPENINGD.*", ".*CHANNELD_NORMAL.*"])
  hClose h


timeout :: MonadUnliftIO m => Int -> ByteString -> m (TailResult a) -> m (TailResult a)
timeout tout msg io = do
  mres <- UIO.timeout tout io
  return (fromMaybe (Left (timeoutError msg)) mres)

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

timeoutError :: ByteString -> TailError
timeoutError = tailErrStr Timeout

noMatchError :: ByteString -> TailError
noMatchError = tailErrStr MatchError

waitForLogs :: MonadIO m => Handle -> [ByteString] -> m ()
waitForLogs handle bs =
  evalTailable t (waitForLogsM bs)
  where
    t = defaultTailable handle

waitForLog :: MonadIO m => Handle -> ByteString -> m ()
waitForLog handle bs = waitForLogs handle [bs]
