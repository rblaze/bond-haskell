module Data.Bond.Proto where

import Data.Bond.Internal.Protocol
import qualified Data.ByteString.Lazy as BL

class BondProto t
class BondTaggedProto t

bondMarshal' :: (BondProto t, BondStruct a) => t -> a -> Either String BL.ByteString
