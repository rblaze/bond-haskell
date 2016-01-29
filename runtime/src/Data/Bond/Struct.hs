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
