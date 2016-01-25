module Data.Bond.Struct where

import Data.Bond.Types

import qualified Data.HashMap.Strict as H

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
    | LIST [Value]
    | SET [Value]
    | MAP [(Value, Value)]
    | BONDED (Bonded Struct)
    deriving Show

data Struct = Struct { base :: Maybe Struct, fields :: H.HashMap Ordinal Value }
    deriving Show
