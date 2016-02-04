module Data.Bond (
    BondProto(bondRead, bondWrite, bondReadWithSchema),
    BondTaggedProto(bondReadTagged, bondWriteTagged),
    BondStruct,
    CompactBinaryProto(..),
    CompactBinaryV1Proto(..),
    FastBinaryProto(..),
    JsonProto(..),
    Ordinal(..),
    SimpleBinaryProto(..),
    SimpleBinaryV1Proto(..),
    Struct(..),
    Value(..),
    getSchema,
    getValue,
    validate
  ) where

import Data.Bond.Bonded
import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto
import Data.Bond.Struct
import Data.Bond.Types

-- compile-time schemas API
-- + bondRead :: BondProto t, BondStruct a => t -> BS.ByteString -> Either String a
-- + bondWrite :: BondProto t, BondStruct a => t -> a -> BS.ByteString

-- + bondUnmarshal :: BondStruct a => BS.ByteString -> Either String a
-- + bondMarshal :: BondProto t, BondStruct a => t -> a -> BS.ByteString

-- runtime schema API
-- bondReadWithSchema :: BondProto t => t -> SchemaDef -> BS.ByteString -> Either String Struct
-- bondWriteWithSchema :: BondProto t => t -> SchemaDef -> Struct -> Either String BS.ByteString

-- bondUnmarshalWithSchema :: SchemaDef -> BS.ByteString -> Either String Struct
-- bondMarshalWithSchema :: BondProto t => t -> SchemaDef -> Struct -> Either String BS.ByteString

-- schemaless API
-- + bondReadTagged :: BondProto t => t -> BS.ByteString -> Either String Struct
-- + bondWriteTagged :: BondProto t => t -> Struct -> BS.ByteString

-- + bondUnmarshalTagged :: BS.ByteString -> Either String Struct
-- + bondMarshalTagged :: BondProto t => t -> Struct -> BS.ByteString

-- Bonded API
-- + getValue
-- + castValue

-- + putValue :: a -> Bonded a
-- + marshalValue :: BondProto t, BondStruct a => t -> a -> Bonded b
-- private: transcoder
