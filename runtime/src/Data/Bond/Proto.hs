module Data.Bond.Proto where

import Data.Bond.Struct
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.TypedSchema

import qualified Data.ByteString.Lazy as BL

class BondProto t where
    bondRead :: BondStruct a => t -> BL.ByteString -> Either String a
    bondWrite :: BondStruct a => t -> a -> Either String BL.ByteString
    bondReadWithSchema :: t -> StructSchema -> BL.ByteString -> Either String Struct
    bondWriteWithSchema :: t -> StructSchema -> Struct -> Either String BL.ByteString
    bondMarshal :: BondStruct a => t -> a -> Either String BL.ByteString
    bondMarshal t = fmap (BL.append $ protoSig t) . bondWrite t
    bondMarshalWithSchema :: t -> StructSchema -> Struct -> Either String BL.ByteString
    bondMarshalWithSchema t s = fmap (BL.append $ protoSig t) . bondWriteWithSchema t s
    protoSig :: t -> BL.ByteString

class BondProto t => BondTaggedProto t where
    bondReadTagged :: t -> BL.ByteString -> Either String Struct
    bondWriteTagged :: t -> Struct -> Either String BL.ByteString
    bondMarshalTagged :: t -> Struct -> Either String BL.ByteString
    bondMarshalTagged t = fmap (BL.append $ protoSig t) . bondWriteTagged t

-- XXX workaround for module loops
bondMarshal' :: (BondProto t, BondStruct a) => t -> a -> Either String BL.ByteString
bondMarshal' = bondMarshal
