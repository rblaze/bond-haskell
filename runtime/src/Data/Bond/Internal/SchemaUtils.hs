module Data.Bond.Internal.SchemaUtils where

import Data.Bond.Internal.Protocol
import Data.Bond.Internal.TypedSchema
import Data.Bond.Schema.BondDataType

import Data.Proxy

bondTypeName :: BondDataType -> String
bondTypeName t
    | t == bT_BOOL = "bool"
    | t == bT_UINT8 = "uint8"
    | t == bT_UINT16 = "uint16"
    | t == bT_UINT32 = "uint32"
    | t == bT_UINT64 = "uint64"
    | t == bT_FLOAT = "float"
    | t == bT_DOUBLE = "double"
    | t == bT_STRING = "string"
    | t == bT_STRUCT = "struct"
    | t == bT_LIST = "list"
    | t == bT_SET = "set"
    | t == bT_MAP = "map"
    | t == bT_INT8 = "int8"
    | t == bT_INT16 = "int16"
    | t == bT_INT32 = "int32"
    | t == bT_INT64 = "int64"
    | t == bT_WSTRING = "wstring"
    | otherwise = let BondDataType v = t in "tag " ++ show v

elementToBondDataType :: ElementTypeInfo -> BondDataType
elementToBondDataType ElementBool = bT_BOOL
elementToBondDataType ElementInt8 = bT_INT8
elementToBondDataType ElementInt16 = bT_INT16
elementToBondDataType ElementInt32 = bT_INT32
elementToBondDataType ElementInt64 = bT_INT64
elementToBondDataType ElementUInt8 = bT_UINT8
elementToBondDataType ElementUInt16 = bT_UINT16
elementToBondDataType ElementUInt32 = bT_UINT32
elementToBondDataType ElementUInt64 = bT_UINT64
elementToBondDataType ElementFloat = bT_FLOAT
elementToBondDataType ElementDouble = bT_DOUBLE
elementToBondDataType ElementString = bT_STRING
elementToBondDataType ElementWString = bT_WSTRING
elementToBondDataType (ElementStruct _) = bT_STRUCT
elementToBondDataType (ElementBonded _) = bT_STRUCT
elementToBondDataType (ElementList _) = bT_LIST
elementToBondDataType (ElementSet _) = bT_SET
elementToBondDataType (ElementMap _ _) = bT_MAP

getWireType :: BondType a => Proxy a -> BondDataType
getWireType = elementToBondDataType . getElementType
