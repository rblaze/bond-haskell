{-|
Bond is an extensible framework for working with schematized data. It is
suitable for scenarios ranging from service communications to Big Data storage
and processing.

Bond defines a rich type system and schema versioning rules which allow
forward and backward compatibility.

Core bond library is published on GitHub at https://github.com/Microsoft/bond/.
-}
module Data.Bond
    ( 
    -- * Example
    -- $examples
    -- * Protocol types
      BondProto(..)
    , BondTaggedProto(..)
    -- * Supported protocols
    , CompactBinaryV1Proto(..)
    , CompactBinaryProto(..)
    , FastBinaryProto(..)
    , SimpleBinaryV1Proto(..)
    , SimpleBinaryProto(..)
    , JsonProto(..)
    -- * bonded\<T>
    , Bonded(..)
    , getValue
    , putValue
    , castValue
    , marshalValue
    , BondedException(..)
    -- * Runtime-schema operations
    -- |Generic applications may need to work with Bond schemas unknown
    -- at compile-time. In order to address such scenarios Bond defines a
    -- type 'Data.Bond.Schema.SchemaDef' to represent schemas in stoorage
    -- and transfer.
    --
    -- Haskell library uses 'StructSchema' internally for performance
    -- reasons and provides conversion functions.
    , BondStruct(getSchema)
    , BondEnum(..)
    , assembleSchema
    , checkStructSchema
    , defaultStruct
    , parseSchema
    , Struct(..)
    , Value(..)
    -- * Marshalling
    -- |Since Bond supports multiple serialization protocols, application
    -- endpoints either have to agree on a particular protocol, or include
    -- protocol metadata in the payload. Marshaling APIs provide the
    -- standard way to do the latter, by automatically adding a payload
    -- header with the protocol identifier and version.
    --
    -- See 'bondMarshal', 'bondMarshalWithSchema' and 'bondMarshalTagged' for serialization.
    , bondUnmarshal
    , bondUnmarshalWithSchema
    , bondUnmarshalTagged
    -- * Misc
    , EncodedString(..)
    , Ordinal(..)
    , defaultValue
    -- | Reexported from generated code
    , BondDataType(..)
    , bT_BOOL, bT_UINT8, bT_UINT16, bT_UINT32, bT_UINT64
    , bT_FLOAT, bT_DOUBLE, bT_STRING, bT_STRUCT, bT_LIST
    , bT_SET, bT_MAP, bT_INT8, bT_INT16, bT_INT32, bT_INT64, bT_WSTRING
    , SchemaDef
    , Void (..)
    ) where

import Data.Bond.Marshal
import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.Types
import Data.Bond.Internal.Bonded
import Data.Bond.Internal.CompactBinaryProto
import Data.Bond.Internal.Default
import Data.Bond.Internal.Enum
import Data.Bond.Internal.FastBinaryProto
import Data.Bond.Internal.JsonProto
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.SchemaOps
import Data.Bond.Internal.SimpleBinaryProto
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.Void

-- $examples
-- Let's use following @schema.bond@ IDL file:
--
-- @
-- namespace my.test
--
-- struct my_struct {
--   10: int32 m_int;
--   20: string m_str = "some string";
-- }
-- @
--
-- Code generation requires @hbc@ program from @bond-haskell-compiler@ package:
--
-- > hbc schema.bond
--
-- This creates file @My.Test.My_struct.hs@. Note that case conversions are 
-- performed to create syntactically correct Haskell code.
--
-- @
-- -- create structure and set m_int to 5:
-- let struct = defaultValue { m_int = 5 }
-- -- serialize @struct@ with FastBinary protocol
-- let Right binstream = bondWrite FastBinaryProto struct
-- -- parse @binstream@ using runtime schema
-- let Right rtstruct = bondReadWithSchema FastBinaryProto (getSchema (Proxy :: Proxy My_struct)) binstream
-- @
