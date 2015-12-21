{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ConstraintKinds #-}
module Data.Bond.Proto (
        BondSerializable(..),
        BondStruct(..),
        BondProto(..),
        ProtoR,
        ProtoW
  ) where

import Data.Bond.Default
import Data.Bond.Monads
import {-# SOURCE #-} Data.Bond.Schema
import Data.Bond.Types
import Data.Bond.Wire
import Data.Hashable
import Data.Proxy
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- Functor context below is for ghc 7.8 (lts-2.22) which can't get it from Monad
type ProtoR t m = (BondProto t, Functor m, Monad m, ReaderM t ~ m)
type ProtoW t m = (BondProto t, Monad m, WriterM t ~ m)

class (Default a, WireType a) => BondSerializable a where
    -- | Read field value.
    bondGet :: ProtoR t m => BondGet t (ReaderM t) a
    -- | Put field into stream.
    bondPut :: BondProto t => a -> BondPut t (WriterM t)

class (Default a, BondSerializable a, Schemable a) => BondStruct a where
    -- | Read struct from untagged stream
    bondStructGetUntagged :: ProtoR t m => BondGet t (ReaderM t) a
    bondStructGetBase :: ProtoR t m => a -> BondGet t (ReaderM t) a
    bondStructGetField :: ProtoR t m => Ordinal -> a -> BondGet t (ReaderM t) a
    -- | Put struct
    bondStructPut :: ProtoW t m => a -> BondPut t (WriterM t)

class BondProto t where
    type ReaderM t :: * -> *
    type WriterM t :: * -> *
    -- | run decoder
    bondDecode :: BondStruct a => Proxy t -> BS.ByteString -> Either String a
    -- | run encoder
    bondEncode :: BondStruct a => Proxy t -> a -> Either String BS.ByteString
    -- | encode top-level struct
    bondPutStruct :: BondStruct a => a -> BondPut t (WriterM t)
    -- | encode base struct
    bondPutBaseStruct :: BondStruct a => a -> BondPut t (WriterM t)
    -- | decode top-level struct
    bondGetStruct :: BondStruct a => BondGet t (ReaderM t) a
    -- | decode top-level struct
    bondGetBaseStruct :: BondStruct a => BondGet t (ReaderM t) a

    bondPutField :: BondSerializable a => Ordinal -> a -> BondPut t (WriterM t)
    bondPutDefNothingField :: BondSerializable a => Ordinal -> Maybe a -> BondPut t (WriterM t)

    bondPutBool :: Bool -> BondPut t (WriterM t)
    bondPutUInt8 :: Word8 -> BondPut t (WriterM t)
    bondPutUInt16 :: Word16 -> BondPut t (WriterM t)
    bondPutUInt32 :: Word32 -> BondPut t (WriterM t)
    bondPutUInt64 :: Word64 -> BondPut t (WriterM t)
    bondPutInt8 :: Int8 -> BondPut t (WriterM t)
    bondPutInt16 :: Int16 -> BondPut t (WriterM t)
    bondPutInt32 :: Int32 -> BondPut t (WriterM t)
    bondPutInt64 :: Int64 -> BondPut t (WriterM t)
    bondPutFloat :: Float -> BondPut t (WriterM t)
    bondPutDouble :: Double -> BondPut t (WriterM t)
    bondPutString :: Utf8 -> BondPut t (WriterM t)
    bondPutWString :: Utf16 -> BondPut t (WriterM t)
    bondPutBlob :: Blob -> BondPut t (WriterM t)
    bondPutList :: BondSerializable a => [a] -> BondPut t (WriterM t)
    bondPutVector :: BondSerializable a => V.Vector a -> BondPut t (WriterM t)
    bondPutHashSet :: BondSerializable a => H.HashSet a -> BondPut t (WriterM t)
    bondPutSet :: BondSerializable a => S.Set a -> BondPut t (WriterM t)
    bondPutMap :: (BondSerializable k, BondSerializable v) => M.Map k v -> BondPut t (WriterM t)
    bondPutNullable :: BondSerializable a => Maybe a -> BondPut t (WriterM t)
    bondPutBonded :: BondStruct a => Bonded a -> BondPut t (WriterM t)

    bondGetBool :: BondGet t (ReaderM t) Bool
    bondGetUInt8 :: BondGet t (ReaderM t) Word8
    bondGetUInt16 :: BondGet t (ReaderM t) Word16
    bondGetUInt32 :: BondGet t (ReaderM t) Word32
    bondGetUInt64 :: BondGet t (ReaderM t) Word64
    bondGetInt8 :: BondGet t (ReaderM t) Int8
    bondGetInt16 :: BondGet t (ReaderM t) Int16
    bondGetInt32 :: BondGet t (ReaderM t) Int32
    bondGetInt64 :: BondGet t (ReaderM t) Int64
    bondGetFloat :: BondGet t (ReaderM t) Float
    bondGetDouble :: BondGet t (ReaderM t) Double
    bondGetString :: BondGet t (ReaderM t) Utf8
    bondGetWString :: BondGet t (ReaderM t) Utf16
    bondGetBlob :: BondGet t (ReaderM t) Blob
    bondGetList :: BondSerializable a => BondGet t (ReaderM t) [a]
    bondGetVector :: BondSerializable a => BondGet t (ReaderM t) (V.Vector a)
    bondGetHashSet :: (Eq a, Hashable a, BondSerializable a) => BondGet t (ReaderM t) (H.HashSet a)
    bondGetSet :: (Ord a, BondSerializable a) => BondGet t (ReaderM t) (S.Set a)
    bondGetMap :: (Ord k, BondSerializable k, BondSerializable v) => BondGet t (ReaderM t) (M.Map k v)
    bondGetNullable :: BondSerializable a => BondGet t (ReaderM t) (Maybe a)
    bondGetDefNothing :: BondSerializable a => BondGet t (ReaderM t) (Maybe a)
    bondGetBonded :: BondStruct a => BondGet t (ReaderM t) (Bonded a)

instance BondSerializable Float where
    bondGet = bondGetFloat
    bondPut = bondPutFloat

instance BondSerializable Double where
    bondGet = bondGetDouble
    bondPut = bondPutDouble

instance BondSerializable Bool where
    bondGet = bondGetBool
    bondPut = bondPutBool

instance BondSerializable Int8 where
    bondGet = bondGetInt8
    bondPut = bondPutInt8

instance BondSerializable Int16 where
    bondGet = bondGetInt16
    bondPut = bondPutInt16

instance BondSerializable Int32 where
    bondGet = bondGetInt32
    bondPut = bondPutInt32

instance BondSerializable Int64 where
    bondGet = bondGetInt64
    bondPut = bondPutInt64

instance BondSerializable Word8 where
    bondGet = bondGetUInt8
    bondPut = bondPutUInt8

instance BondSerializable Word16 where
    bondGet = bondGetUInt16
    bondPut = bondPutUInt16

instance BondSerializable Word32 where
    bondGet = bondGetUInt32
    bondPut = bondPutUInt32

instance BondSerializable Word64 where
    bondGet = bondGetUInt64
    bondPut = bondPutUInt64

instance BondSerializable Utf8 where
    bondGet = bondGetString
    bondPut = bondPutString

instance BondSerializable Utf16 where
    bondGet = bondGetWString
    bondPut = bondPutWString

instance BondSerializable Blob where
    bondGet = bondGetBlob
    bondPut = bondPutBlob

instance BondSerializable a => BondSerializable [a] where
    bondGet = bondGetList
    bondPut = bondPutList

instance BondSerializable a => BondSerializable (V.Vector a) where
    bondGet = bondGetVector
    bondPut = bondPutVector

instance (Eq a, Hashable a, BondSerializable a) => BondSerializable (H.HashSet a) where
    bondGet = bondGetHashSet
    bondPut = bondPutHashSet

instance (Ord a, BondSerializable a) => BondSerializable (S.Set a) where
    bondGet = bondGetSet
    bondPut = bondPutSet

instance (Ord k, BondSerializable k, BondSerializable v) => BondSerializable (M.Map k v) where
    bondGet = bondGetMap
    bondPut = bondPutMap

instance BondStruct a => BondSerializable (Bonded a) where
    bondGet = bondGetBonded
    bondPut = bondPutBonded

instance BondSerializable a => BondSerializable (Maybe a) where
    bondGet = bondGetNullable
    bondPut = bondPutNullable
