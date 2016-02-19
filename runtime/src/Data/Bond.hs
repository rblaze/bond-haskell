module Data.Bond (
    BondProto(bondRead, bondWrite, bondReadWithSchema, bondWriteWithSchema),
    BondTaggedProto(bondReadTagged, bondWriteTagged),
    BondStruct(getSchema),
    CompactBinaryProto(..),
    CompactBinaryV1Proto(..),
    EncodedString(..),
    FastBinaryProto(..),
    JsonProto(..),
    Ordinal(..),
    SimpleBinaryProto(..),
    SimpleBinaryV1Proto(..),
    Struct(..),
    Value(..),
    defaultValue,
    checkStructSchema,
    getValue,
    marshalValue,
    putValue
  ) where

import Data.Bond.Bonded
import Data.Bond.CompactBinaryProto
import Data.Bond.Default
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto
import Data.Bond.Struct
import Data.Bond.Types
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.SchemaOps

-- * compile-time schemas API
-- | 'bondRead'
-- 'bondWrite'
-- 'bondUnmarshal'
-- 'bondMarshal'

-- * runtime schema API
-- | 'bondReadWithSchema'
-- 'bondWriteWithSchema'
-- 'bondUnmarshalWithSchema'
-- 'bondMarshalWithSchema'

-- * schemaless API
-- | 'bondReadTagged'
-- 'bondWriteTagged'
-- 'bondUnmarshalTagged'
-- 'bondMarshalTagged'

-- * Bonded API
-- | 'getValue'
-- 'castValue'
-- 'putValue'
-- 'marshalValue'
