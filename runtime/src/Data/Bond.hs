{-# Language ScopedTypeVariables #-}
module Data.Bond (
    BondProto,
    BondStruct,
    CompactBinaryProto,
    CompactBinaryV1Proto,
    FastBinaryProto,
    JsonProto,
    SimpleBinaryProto,
    SimpleBinaryV1Proto,
    bondRead,
    bondWrite,
    getSchema
  ) where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto

import Data.Proxy
import qualified Data.ByteString.Lazy as L

bondRead :: forall a t . (BondStruct a, BondProto t) => Proxy t -> L.ByteString -> Either String a
bondRead = bondDecode

bondWrite :: forall a t . (BondStruct a, BondProto t) => Proxy t -> a -> Either String L.ByteString
bondWrite = bondEncode
