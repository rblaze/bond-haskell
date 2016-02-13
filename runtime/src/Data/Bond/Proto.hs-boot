module Data.Bond.Proto where

import qualified Data.ByteString.Lazy as BL

class BondProto t
class BondTaggedProto t
class BondStruct a

bondMarshal' :: (BondProto t, BondStruct a) => t -> a -> Either String BL.ByteString
