module Data.Bond.Proto where

import qualified Data.ByteString.Lazy as BL

data Struct

class BondProto t
class BondStruct a

bondMarshal' :: (BondProto t, BondStruct a) => t -> a -> BL.ByteString
