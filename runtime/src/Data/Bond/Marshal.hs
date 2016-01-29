{-# LANGUAGE MultiWayIf #-}
module Data.Bond.Marshal (
    BondProto(bondMarshal),
    BondTaggedProto(bondMarshalTagged),
    bondUnmarshal,
    bondUnmarshalTagged
  ) where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto
import Data.Bond.Struct

import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondUnmarshal s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondRead FastBinaryProto rest
             | sig == protoSig CompactBinaryProto -> bondRead CompactBinaryProto rest
             | sig == protoSig CompactBinaryV1Proto -> bondRead CompactBinaryV1Proto rest
             | sig == protoSig SimpleBinaryProto -> bondRead SimpleBinaryProto rest
             | sig == protoSig SimpleBinaryV1Proto -> bondRead SimpleBinaryV1Proto rest
             | sig == protoSig JsonProto -> bondRead JsonProto rest
             | otherwise -> Left "unknown signature in marshalled stream"

bondUnmarshalTagged :: BL.ByteString -> Either String Struct
bondUnmarshalTagged s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondReadTagged FastBinaryProto rest
             | sig == protoSig CompactBinaryProto -> bondReadTagged CompactBinaryProto rest
             | sig == protoSig CompactBinaryV1Proto -> bondReadTagged CompactBinaryV1Proto rest
             | sig == protoSig SimpleBinaryProto -> Left "SimpleBinaryProto does not support schemaless operations"
             | sig == protoSig SimpleBinaryV1Proto -> Left "SimpleBinaryV1Proto does not support schemaless operations"
             | sig == protoSig JsonProto -> Left "JsonProto does not support schemaless operations"
             | otherwise -> Left "unknown signature in marshalled stream"
