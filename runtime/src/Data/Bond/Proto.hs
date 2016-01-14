{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Bond.Proto (
        Serializable(..),
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
import Control.Monad.Reader.Class
import Control.Monad.State.Class
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
deriving instance (MonadReader r (ReaderM t)) => MonadReader r (BondGet t)
deriving instance (MonadState s (ReaderM t)) => MonadState s (BondGet t)

newtype BondPutM t a = BondPut ((WriterM t) a)
deriving instance (Functor (WriterM t)) => Functor (BondPutM t)
deriving instance (Applicative (WriterM t)) => Applicative (BondPutM t)
deriving instance (Monad (WriterM t)) => Monad (BondPutM t)
deriving instance (MonadReader r (WriterM t)) => MonadReader r (BondPutM t)
deriving instance (MonadState s (WriterM t)) => MonadState s (BondPutM t)

type BondPut t = BondPutM t ()

class (Default a, WireType a) => Serializable a where
    -- | Read field value.
    bondGet :: (Functor (ReaderM t), Monad (ReaderM t), BondProto t) => BondGet t a
    -- | Put field into stream.
    bondPut :: (Monad (WriterM t), BondProto t) => a -> BondPut t

class (Default a, Serializable a, Schemable a) => BondStruct a where
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

    bondPutField :: Serializable a => Ordinal -> a -> BondPut t
    bondPutDefNothingField :: Serializable a => Ordinal -> Maybe a -> BondPut t

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
    bondPutList :: Serializable a => [a] -> BondPut t
    bondPutVector :: Serializable a => V.Vector a -> BondPut t
    bondPutHashSet :: Serializable a => H.HashSet a -> BondPut t
    bondPutSet :: Serializable a => S.Set a -> BondPut t
    bondPutMap :: (Serializable k, Serializable v) => M.Map k v -> BondPut t
    bondPutNullable :: Serializable a => Maybe a -> BondPut t
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
    bondGetList :: Serializable a => BondGet t [a]
    bondGetVector :: Serializable a => BondGet t (V.Vector a)
    bondGetHashSet :: (Eq a, Hashable a, Serializable a) => BondGet t (H.HashSet a)
    bondGetSet :: (Ord a, Serializable a) => BondGet t (S.Set a)
    bondGetMap :: (Ord k, Serializable k, Serializable v) => BondGet t (M.Map k v)
    bondGetNullable :: Serializable a => BondGet t (Maybe a)
    bondGetDefNothing :: Serializable a => BondGet t (Maybe a)
    bondGetBonded :: BondStruct a => BondGet t (Bonded a)

instance Serializable Float where
    bondGet = bondGetFloat
    bondPut = bondPutFloat

instance Serializable Double where
    bondGet = bondGetDouble
    bondPut = bondPutDouble

instance Serializable Bool where
    bondGet = bondGetBool
    bondPut = bondPutBool

instance Serializable Int8 where
    bondGet = bondGetInt8
    bondPut = bondPutInt8

instance Serializable Int16 where
    bondGet = bondGetInt16
    bondPut = bondPutInt16

instance Serializable Int32 where
    bondGet = bondGetInt32
    bondPut = bondPutInt32

instance Serializable Int64 where
    bondGet = bondGetInt64
    bondPut = bondPutInt64

instance Serializable Word8 where
    bondGet = bondGetUInt8
    bondPut = bondPutUInt8

instance Serializable Word16 where
    bondGet = bondGetUInt16
    bondPut = bondPutUInt16

instance Serializable Word32 where
    bondGet = bondGetUInt32
    bondPut = bondPutUInt32

instance Serializable Word64 where
    bondGet = bondGetUInt64
    bondPut = bondPutUInt64

instance Serializable Utf8 where
    bondGet = bondGetString
    bondPut = bondPutString

instance Serializable Utf16 where
    bondGet = bondGetWString
    bondPut = bondPutWString

instance Serializable Blob where
    bondGet = bondGetBlob
    bondPut = bondPutBlob

instance Serializable a => Serializable [a] where
    bondGet = bondGetList
    bondPut = bondPutList

instance Serializable a => Serializable (V.Vector a) where
    bondGet = bondGetVector
    bondPut = bondPutVector

instance (Eq a, Hashable a, Serializable a) => Serializable (H.HashSet a) where
    bondGet = bondGetHashSet
    bondPut = bondPutHashSet

instance (Ord a, Serializable a) => Serializable (S.Set a) where
    bondGet = bondGetSet
    bondPut = bondPutSet

instance (Ord k, Serializable k, Serializable v) => Serializable (M.Map k v) where
    bondGet = bondGetMap
    bondPut = bondPutMap

instance BondStruct a => Serializable (Bonded a) where
    bondGet = bondGetBonded
    bondPut = bondPutBonded

instance Serializable a => Serializable (Maybe a) where
    bondGet = bondGetNullable
    bondPut = bondPutNullable
