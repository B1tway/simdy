module StringUtils(toShort') where

-- String stuff
import qualified Data.ByteString.UTF8 as BSU
import Data.ByteString.Short (ShortByteString, toShort)

toShort' :: String -> ShortByteString
toShort' = toShort . BSU.fromString

