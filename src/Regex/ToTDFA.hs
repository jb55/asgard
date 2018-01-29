
module Regex.ToTDFA where

import Text.Regex.TDFA.ByteString (Regex)
import Text.Regex.TDFA (makeRegex)
import Data.ByteString (ByteString)

bstr :: ByteString -> ByteString
bstr x = x

class ToTDFA a where
    toTDFA :: a -> Regex

instance ToTDFA Regex where
    toTDFA r = r

instance ToTDFA ByteString where
    toTDFA bs = makeRegex bs
