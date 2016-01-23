module Data.Bond (
    BondProto(bondRead, bondWrite),
    BondStruct,
    CompactBinaryProto(..),
    CompactBinaryV1Proto(..),
    FastBinaryProto(..),
    JsonProto(..),
    SimpleBinaryProto(..),
    SimpleBinaryV1Proto(..),
    getSchema,
    getValue
  ) where

import Data.Bond.Bonded
import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto

-- compile-time schemas API
-- + bondRead :: BondProto t, BondStruct a => t -> BS.ByteString -> Either String a
-- + bondWrite :: BondProto t, BondStruct a => t -> a -> BS.ByteString

-- + bondUnmarshal :: BondStruct a => BS.ByteString -> Either String a
-- + bondMarshal :: BondProto t, BondStruct a => t -> a -> BS.ByteString

-- runtime schema API
-- bondReadWithSchema :: BondProto t => t -> SchemaDef -> BS.ByteString -> Either String Struct
-- bondWriteWithSchema :: BondProto t => t -> SchemaDef -> Struct -> BS.ByteString

-- bondUnmarshalWithSchema :: SchemaDef -> BS.ByteString -> Either String Struct
-- bondMarshalWithSchema :: BondProto t => t -> SchemaDef -> Struct -> BS.ByteString

-- schemaless API
-- bondReadTagged :: BondProto t => t -> BS.ByteString -> Either String Struct
-- bondPutTagged :: BondProto t => t -> Struct -> BS.ByteString

-- bondUnmarshalTagged :: BS.ByteString -> Either String Struct
-- bondMarshalTagged :: BondProto t => t -> Struct -> BS.ByteString
