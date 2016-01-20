module Data.Bond.Marshal where

import {-# SOURCE #-} Data.Bond.Proto
import qualified Data.ByteString.Lazy as L

bondReadMarshalled :: BondStruct a => L.ByteString -> Either String a
bondWriteMarshalled :: (BondStruct a, BondProto t) => t -> a -> Either String L.ByteString
