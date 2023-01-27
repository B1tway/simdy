module StringUtils (toShort') where

import Data.ByteString.Short (ShortByteString, toShort)

-- String stuff
import qualified Data.ByteString.UTF8 as BSU

toShort' :: String -> ShortByteString
toShort' = toShort . BSU.fromString
