module Data.Bond.Utils where

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.ProtocolType

import Control.Monad
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BL

parseHeader :: BL.ByteString -> (ProtocolType, Word16)
parseHeader s | BL.length s /= 4 = (ProtocolType maxBound, maxBound)
parseHeader s = (protoSig, protoVer)
    where
    [s0, s1, v0, v1] = BL.unpack $ BL.take 4 s
    protoSig = ProtocolType $ fromIntegral s0 .|. (fromIntegral s1 `shiftL` 8)
    protoVer = fromIntegral v0 .|. (fromIntegral v1 `shiftL` 8)

protoHeader :: ProtocolType -> Word16 -> BL.ByteString
protoHeader (ProtocolType protoSig) protoVer = BL.pack [s0, s1, v0, v1]
    where
    s0 = fromIntegral protoSig
    s1 = fromIntegral (protoSig `shiftR` 8)
    v0 = fromIntegral protoVer
    v1 = fromIntegral (protoVer `shiftR` 8)

checkType :: Monad f => String -> BondDataType -> BondDataType -> f ()
checkType name schemaType streamType =
    unless (schemaType == streamType) $
        fail $ name ++ " type mismatch: " ++ show schemaType ++ " expected, " ++ show streamType ++ " found"

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
