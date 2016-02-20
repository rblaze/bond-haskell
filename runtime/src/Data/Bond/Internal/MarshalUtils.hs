module Data.Bond.Internal.MarshalUtils where

import Data.Bond.Marshal
import Data.Bond.Internal.Protocol

import qualified Data.ByteString.Lazy as BL

-- XXX workaround for module loops
bondMarshal' :: (BondProto t, BondStruct a) => t -> a -> Either String BL.ByteString
bondMarshal' = bondMarshal
