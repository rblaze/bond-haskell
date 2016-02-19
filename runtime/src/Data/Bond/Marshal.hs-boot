module Data.Bond.Marshal where

import Data.Bond.Internal.Protocol
import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
