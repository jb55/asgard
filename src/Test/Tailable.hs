{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tailable where

import Control.Monad (unless, foldM_)
import Text.Regex.TDFA.ByteString (Regex)
import Text.Regex.Base.RegexLike (match, matchTest, makeRegex)
import System.IO (Handle, openFile, IOMode(ReadMode), hClose)
import System.Timeout (timeout)
import Data.ByteString (hGetLine, ByteString)

data Tailable =
  Tailable {
      tailHandle :: Handle
    }

untilM_ :: (Monad m) => m Bool -> m ()
untilM_ act = do
  b <- act
  unless b (untilM_ act)

foldM1_ :: (Foldable t, Monad m) => (a -> m ()) -> t a -> m ()
foldM1_ folder xs = foldM_ (\() x -> folder x) () xs

-- keep reading lines from a handle until it matches all regexes in order
waitForLogs :: Handle -> [ByteString] -> IO ()
waitForLogs handle rs = foldM1_ folder regexes
  where
    regexes      = map makeRegex rs :: [Regex]
    readline     = hGetLine handle
    folder regex = untilM_ (matchTest regex <$> readline)

test :: IO ()
test = do
  writeFile "/tmp/haskelltest" "HI\nderp OPENINGD hey\nCHANNELD_NORMAL"
  h <- openFile "/tmp/haskelltest" ReadMode
  timeout 100000 (waitForLogs h [".*OPENINGD.*", ".*CHANNELD_NORMAL.*"])
  hClose h
