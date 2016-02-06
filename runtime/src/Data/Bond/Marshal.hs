{-# LANGUAGE MultiWayIf #-}
module Data.Bond.Marshal (
    BondProto(bondMarshal, bondMarshalWithSchema),
    BondTaggedProto(bondMarshalTagged),
    bondUnmarshal,
    bondUnmarshalWithSchema,
    bondUnmarshalTagged
  ) where

import Data.Bond.Schema.SchemaDef

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

bondUnmarshalWithSchema :: SchemaDef -> BL.ByteString -> Either String Struct
bondUnmarshalWithSchema schema s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondReadWithSchema FastBinaryProto schema rest
             | sig == protoSig CompactBinaryProto -> bondReadWithSchema CompactBinaryProto schema rest
             | sig == protoSig CompactBinaryV1Proto -> bondReadWithSchema CompactBinaryV1Proto schema rest
             | sig == protoSig SimpleBinaryProto -> bondReadWithSchema SimpleBinaryProto schema rest
             | sig == protoSig SimpleBinaryV1Proto -> bondReadWithSchema SimpleBinaryV1Proto schema rest
             | sig == protoSig JsonProto -> bondReadWithSchema JsonProto schema rest
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
