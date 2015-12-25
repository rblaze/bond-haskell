{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ScopedTypeVariables, TypeFamilies #-}
module Data.Bond.Proto (
        BondSerializable(..),
        BondStruct(..),
        BondProto(..),
        BondGet(..),
        BondPutM(..),
        BondPut
  ) where

import Data.Bond.Default
import {-# SOURCE #-} Data.Bond.Schema
import Data.Bond.Types
import Data.Bond.Wire

import Control.Applicative
import Data.Hashable
import Data.Proxy
import Prelude
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

newtype BondGet t a = BondGet ((ReaderM t) a)
deriving instance (Functor (ReaderM t)) => Functor (BondGet t)
deriving instance (Applicative (ReaderM t)) => Applicative (BondGet t)
deriving instance (Monad (ReaderM t)) => Monad (BondGet t)

newtype BondPutM t a = BondPut ((WriterM t) a)
deriving instance (Functor (WriterM t)) => Functor (BondPutM t)
deriving instance (Applicative (WriterM t)) => Applicative (BondPutM t)
deriving instance (Monad (WriterM t)) => Monad (BondPutM t)

type BondPut t = BondPutM t ()

class (Default a, WireType a) => BondSerializable a where
    -- | Read field value.
    bondGet :: (Functor (ReaderM t), Monad (ReaderM t), BondProto t) => BondGet t a
    -- | Put field into stream.
    bondPut :: (Monad (WriterM t), BondProto t) => a -> BondPut t

class (Default a, BondSerializable a, Schemable a) => BondStruct a where
    -- | Read struct from untagged stream
    bondStructGetUntagged :: (Functor (ReaderM t), Monad (ReaderM t), BondProto t) => BondGet t a
    bondStructGetBase :: (Monad (ReaderM t), BondProto t) => a -> BondGet t a
    bondStructGetField :: (Functor (ReaderM t), Monad (ReaderM t), BondProto t) => Ordinal -> a -> BondGet t a
    -- | Put struct
    bondStructPut :: (Monad (WriterM t), BondProto t) => a -> BondPut t

class BondProto t where
    type ReaderM t :: * -> *
    type WriterM t :: * -> *
    -- | run decoder
    bondDecode :: BondStruct a => Proxy t -> BS.ByteString -> Either String a
    -- | run encoder
    bondEncode :: BondStruct a => Proxy t -> a -> Either String BS.ByteString
    -- | encode top-level struct
    bondPutStruct :: BondStruct a => a -> BondPut t
    -- | encode base struct
    bondPutBaseStruct :: BondStruct a => a -> BondPut t
    -- | decode top-level struct
    bondGetStruct :: BondStruct a => BondGet t a
    -- | decode top-level struct
    bondGetBaseStruct :: BondStruct a => BondGet t a

    bondPutField :: BondSerializable a => Ordinal -> a -> BondPut t
    bondPutDefNothingField :: BondSerializable a => Ordinal -> Maybe a -> BondPut t

    bondPutBool :: Bool -> BondPut t
    bondPutUInt8 :: Word8 -> BondPut t
    bondPutUInt16 :: Word16 -> BondPut t
    bondPutUInt32 :: Word32 -> BondPut t
    bondPutUInt64 :: Word64 -> BondPut t
    bondPutInt8 :: Int8 -> BondPut t
    bondPutInt16 :: Int16 -> BondPut t
    bondPutInt32 :: Int32 -> BondPut t
    bondPutInt64 :: Int64 -> BondPut t
    bondPutFloat :: Float -> BondPut t
    bondPutDouble :: Double -> BondPut t
    bondPutString :: Utf8 -> BondPut t
    bondPutWString :: Utf16 -> BondPut t
    bondPutBlob :: Blob -> BondPut t
    bondPutList :: BondSerializable a => [a] -> BondPut t
    bondPutVector :: BondSerializable a => V.Vector a -> BondPut t
    bondPutHashSet :: BondSerializable a => H.HashSet a -> BondPut t
    bondPutSet :: BondSerializable a => S.Set a -> BondPut t
    bondPutMap :: (BondSerializable k, BondSerializable v) => M.Map k v -> BondPut t
    bondPutNullable :: BondSerializable a => Maybe a -> BondPut t
    bondPutBonded :: BondStruct a => Bonded a -> BondPut t

    bondGetBool :: BondGet t Bool
    bondGetUInt8 :: BondGet t Word8
    bondGetUInt16 :: BondGet t Word16
    bondGetUInt32 :: BondGet t Word32
    bondGetUInt64 :: BondGet t Word64
    bondGetInt8 :: BondGet t Int8
    bondGetInt16 :: BondGet t Int16
    bondGetInt32 :: BondGet t Int32
    bondGetInt64 :: BondGet t Int64
    bondGetFloat :: BondGet t Float
    bondGetDouble :: BondGet t Double
    bondGetString :: BondGet t Utf8
    bondGetWString :: BondGet t Utf16
    bondGetBlob :: BondGet t Blob
    bondGetList :: BondSerializable a => BondGet t [a]
    bondGetVector :: BondSerializable a => BondGet t (V.Vector a)
    bondGetHashSet :: (Eq a, Hashable a, BondSerializable a) => BondGet t (H.HashSet a)
    bondGetSet :: (Ord a, BondSerializable a) => BondGet t (S.Set a)
    bondGetMap :: (Ord k, BondSerializable k, BondSerializable v) => BondGet t (M.Map k v)
    bondGetNullable :: BondSerializable a => BondGet t (Maybe a)
    bondGetDefNothing :: BondSerializable a => BondGet t (Maybe a)
    bondGetBonded :: BondStruct a => BondGet t (Bonded a)

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
