module Data.Bond (
    BondProto(bondRead, bondWrite, bondReadWithSchema, bondWriteWithSchema),
    BondTaggedProto(bondReadTagged, bondWriteTagged),
    BondStruct,
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
    getSchema,
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
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto
import Data.Bond.Struct
import Data.Bond.Types

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
