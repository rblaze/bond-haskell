{-# Language DeriveGeneric #-}
module Data.Bond.Struct where

import {-# SOURCE #-} Data.Bond.Schema.BondDataType
import Data.Bond.Types

import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M

-- |Representation of bond serializable type used in runtime-schema operations.
data Value
    = BOOL Bool
    | INT8 Int8
    | INT16 Int16
    | INT32 Int32
    | INT64 Int64
    | UINT8 Word8
    | UINT16 Word16
    | UINT32 Word32
    | UINT64 Word64
    | FLOAT Float
    | DOUBLE Double
    | STRING Utf8
    | WSTRING Utf16
    | STRUCT Struct
    | LIST BondDataType [Value]
    | SET BondDataType [Value]
    | MAP BondDataType BondDataType [(Value, Value)]
    | BONDED (Bonded Struct)
    deriving (Show, Generic)

instance NFData Value

instance Eq Value where
    (BOOL a) == (BOOL b) = a == b
    (INT8 a) == (INT8 b) = a == b
    (INT16 a) == (INT16 b) = a == b
    (INT32 a) == (INT32 b) = a == b
    (INT64 a) == (INT64 b) = a == b
    (UINT8 a) == (UINT8 b) = a == b
    (UINT16 a) == (UINT16 b) = a == b
    (UINT32 a) == (UINT32 b) = a == b
    (UINT64 a) == (UINT64 b) = a == b
    (FLOAT a) == (FLOAT b) = a == b
    (DOUBLE a) == (DOUBLE b) = a == b
    (STRING a) == (STRING b) = a == b
    (WSTRING a) == (WSTRING b) = a == b
    (STRUCT a) == (STRUCT b) = a == b
    (LIST ta a) == (LIST tb b) = ta == tb && a == b
    (SET ta a) == (SET tb b) = ta == tb && a == b
    (MAP ka va a) == (MAP kb vb b) = ka == kb && va == vb && a == b
    (BONDED (BondedObject a)) == (BONDED (BondedObject b)) = a == b
    (BONDED (BondedStream a)) == (BONDED (BondedStream b)) = a == b
    _ == _ = False

-- |Representation of bond structure used in runtime-schema operations.
data Struct = Struct { base :: Maybe Struct, fields :: M.Map Ordinal Value }
    deriving (Show, Eq, Generic)

instance NFData Struct

valueName :: Value -> String
valueName (BOOL _) = "bool"
valueName (INT8 _) = "int8"
valueName (INT16 _) = "int16"
valueName (INT32 _) = "int32"
valueName (INT64 _) = "int64"
valueName (UINT8 _) = "uint8"
valueName (UINT16 _) = "uint16"
valueName (UINT32 _) = "uint32"
valueName (UINT64 _) = "uint64"
valueName (FLOAT _) = "float"
valueName (DOUBLE _) = "double"
valueName (STRING _) = "string"
valueName (WSTRING _) = "wstring"
valueName (STRUCT _) = "struct"
valueName LIST{} = "list"
valueName SET{} = "set"
valueName MAP{} = "map"
valueName (BONDED _) = "bonded"
