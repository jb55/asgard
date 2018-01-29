{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitcoin.Denomination
    ( Denomination(..)
    , MSats(..)
    , Sats(..)
    , Bits(..)
    , bits, msats, sats, toBits
    ) where

import Data.Aeson
import Text.Printf

class Denomination a where
  toMsats :: a -> Rational
  -- fromMsats :: Int -> a
  -- numMsats :: Int

newtype MSats = MSats { getMsats :: Int }
  deriving (FromJSON, ToJSON, Num, Ord, Eq)

newtype Sats = Sats { getSats :: Rational }
  deriving (FromJSON, ToJSON, Num, Ord, Eq)

newtype Bits = Bits { getBits :: Rational }
  deriving (FromJSON, ToJSON, Num, Ord, Eq)

instance Denomination MSats where
  toMsats (MSats msats_) = toRational msats_

bitsSize :: Num a => a
bitsSize = 100000

instance Denomination Bits where
  toMsats (Bits bitz) = bitz * bitsSize

instance Denomination Sats where
  toMsats (Sats sats_) = sats_ * 1000

bits :: Rational -> Bits
bits = Bits

msats :: Int -> MSats
msats = MSats

sats :: Rational -> Sats
sats = Sats

toBits :: Denomination a => a -> Bits
toBits = Bits . (/ bitsSize) . toRational . toMsats

instance Show MSats where
  show (MSats units) = printf "%d msats" units

instance Show Bits where
  show (Bits units) = printf "%s bits" (showRational 4 units)

instance Show Sats where
  show (Sats units) = printf "%s sats" (showRational 2 units)

showRational :: Int -> Rational -> String
showRational n r =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
