module Data.Bond.Proto where

import Data.Bond.Struct
import Data.Bond.TypedSchema
import Data.Bond.Internal.Protocol

import qualified Data.ByteString.Lazy as BL

-- |Typeclass for Bond serialization protocols.
class BondProto t where
    -- |Deserialize structure from stream.
    bondRead :: BondStruct a => t -> BL.ByteString -> Either String a
    -- |Serialize structure to stream.
    bondWrite :: BondStruct a => t -> a -> Either String BL.ByteString
    -- |Deserialize structure from stream using provided schema.
    bondReadWithSchema :: t -> StructSchema -> BL.ByteString -> Either String Struct
    -- |Serialize structure to stream using provided schema.
    bondWriteWithSchema :: t -> StructSchema -> Struct -> Either String BL.ByteString
    -- |Serialize structure to stream and add protocol header. See 'Data.Bond.Marshal.bondUnmarshal' for deserialization.
    bondMarshal :: BondStruct a => t -> a -> Either String BL.ByteString
    bondMarshal t = fmap (BL.append $ protoSig t) . bondWrite t
    -- |Serialize structure to stream using provided schema and add protocol header. See 'Data.Bond.Marshal.bondUnmarshalWithSchema' for deserialization.
    bondMarshalWithSchema :: t -> StructSchema -> Struct -> Either String BL.ByteString
    bondMarshalWithSchema t s = fmap (BL.append $ protoSig t) . bondWriteWithSchema t s
    -- |Get protocol header.
    protoSig :: t -> BL.ByteString

-- |Typeclass for tagged Bond serialization protocols. Such protocols support schemaless operations.
class BondProto t => BondTaggedProto t where
    -- |Deserialize structure from stream without schema.
    bondReadTagged :: t -> BL.ByteString -> Either String Struct
    -- |Serialize structure to stream without schema.
    bondWriteTagged :: t -> Struct -> Either String BL.ByteString
    -- |Serialize structure to stream without schema and add protocol header. See 'Data.Bond.Marshal.bondUnmarshalTagged' for deserialization.
    bondMarshalTagged :: t -> Struct -> Either String BL.ByteString
    bondMarshalTagged t = fmap (BL.append $ protoSig t) . bondWriteTagged t
