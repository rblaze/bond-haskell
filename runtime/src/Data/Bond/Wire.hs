{-# Language ScopedTypeVariables #-}
module Data.Bond.Wire (
    Ordinal(..),
    WireType(..),
    bT_STRUCT
  ) where

import Data.Bond.Types
import Data.Proxy
import {-# SOURCE #-} Data.Bond.Schema.BondDataType

class WireType a where
    getWireType :: Proxy a -> BondDataType

instance WireType Bool where getWireType _ = bT_BOOL
instance WireType Double where getWireType _ = bT_DOUBLE
instance WireType Float where getWireType _ = bT_FLOAT
instance WireType Int8 where getWireType _ = bT_INT8
instance WireType Int16 where getWireType _ = bT_INT16
instance WireType Int32 where getWireType _ = bT_INT32
instance WireType Int64 where getWireType _ = bT_INT64
instance WireType Word8 where getWireType _ = bT_UINT8
instance WireType Word16 where getWireType _ = bT_UINT16
instance WireType Word32 where getWireType _ = bT_UINT32
instance WireType Word64 where getWireType _ = bT_UINT64
instance WireType (Maybe a) where getWireType _ = bT_LIST
instance WireType [a] where getWireType _ = bT_LIST
instance WireType Blob where getWireType _ = bT_LIST
instance WireType Utf8 where getWireType _ = bT_STRING
instance WireType Utf16 where getWireType _ = bT_WSTRING
instance WireType (Map a b) where getWireType _ = bT_MAP
instance WireType (HashSet a) where getWireType _ = bT_SET
instance WireType (Set a) where getWireType _ = bT_SET
instance WireType (Vector a) where getWireType _ = bT_LIST
instance WireType (Bonded a) where getWireType _ = bT_STRUCT

newtype Ordinal = Ordinal Word16
    deriving (Eq, Ord, Show)
