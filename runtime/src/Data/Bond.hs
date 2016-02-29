module Data.Bond
    ( BondProto(bondRead, bondWrite, bondReadWithSchema, bondWriteWithSchema)
    , BondStruct(getSchema)
    , BondTaggedProto(bondReadTagged, bondWriteTagged)
    , CompactBinaryProto(..)
    , CompactBinaryV1Proto(..)
    , EncodedString(..)
    , FastBinaryProto(..)
    , JsonProto(..)
    , Ordinal(..)
    , SimpleBinaryProto(..)
    , SimpleBinaryV1Proto(..)
    , Struct(..)
    , Value(..)
    , assembleSchema
    , checkStructSchema
    , defaultValue
    , defaultStruct
    , getValue
    , marshalValue
    , parseSchema
    , putValue
    ) where

import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.Types
import Data.Bond.Internal.Bonded
import Data.Bond.Internal.CompactBinaryProto
import Data.Bond.Internal.Default
import Data.Bond.Internal.FastBinaryProto
import Data.Bond.Internal.JsonProto
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.SchemaOps
import Data.Bond.Internal.SimpleBinaryProto

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
