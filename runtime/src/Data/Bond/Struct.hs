module Data.Bond.Struct where

import {-# SOURCE #-} Data.Bond.Schema.BondDataType
import Data.Bond.Types

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

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
    | BONDED BS.ByteString -- (Bonded Struct)
    deriving (Show, Eq)

data Struct = Struct { base :: Maybe Struct, fields :: M.Map Ordinal Value }
    deriving (Show, Eq)

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
valueName (LIST _ _) = "list"
valueName (SET _ _) = "set"
valueName (MAP _ _ _) = "map"
valueName (BONDED _) = "bonded"
