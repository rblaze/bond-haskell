module Data.Bond (
    BondedException,
    BondProto,
    BondStruct,
    CompactBinaryProto(..),
    CompactBinaryV1Proto(..),
    FastBinaryProto(..),
    JsonProto(..),
    SimpleBinaryProto(..),
    SimpleBinaryV1Proto(..),
    bondRead,
    bondReadMarshalled,
    bondWrite,
    bondWriteMarshalled,
    castValue,
    getSchema,
    getValue
  ) where

import Data.Bond.Bonded
import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Marshal
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto

import qualified Data.ByteString.Lazy as L

bondRead :: (BondStruct a, BondProto t) => t -> L.ByteString -> Either String a
bondRead = bondDecode

bondWrite :: (BondStruct a, BondProto t) => t -> a -> Either String L.ByteString
bondWrite = bondEncode
