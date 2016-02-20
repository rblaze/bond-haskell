{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Data.Bond.Internal.Protocol where

import Data.Bond.TypedSchema
import Data.Bond.Types
import Data.Bond.Internal.BinaryClass
import Data.Bond.Internal.Default
import Data.Bond.Internal.Utils

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Hashable
import Data.Proxy
import Data.Text
import Prelude          -- ghc 7.10 workaround for Control.Applicative
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
deriving instance (MonadError e (ReaderM t)) => MonadError e (BondGet t)

newtype BondPutM t a = BondPut ((WriterM t) a)
deriving instance (Functor (WriterM t)) => Functor (BondPutM t)
deriving instance (Applicative (WriterM t)) => Applicative (BondPutM t)
deriving instance (Monad (WriterM t)) => Monad (BondPutM t)
deriving instance (MonadReader r (WriterM t)) => MonadReader r (BondPutM t)
deriving instance (MonadState s (WriterM t)) => MonadState s (BondPutM t)
deriving instance (MonadError e (WriterM t)) => MonadError e (BondPutM t)
deriving instance (BinaryPut (WriterM t)) => BinaryPut (BondPutM t)

type BondPut t = BondPutM t ()

class Default a => BondType a where
    -- | Read value.
    bondGet :: (Functor (ReaderM t), Monad (ReaderM t), Protocol t) => BondGet t a
    -- | Write value.
    bondPut :: (Monad (BondPutM t), Protocol t) => a -> BondPut t
    -- | Get name of type.
    getName :: Proxy a -> Text
    -- | Get qualified name of type.
    getQualifiedName :: Proxy a -> Text
    -- | Get ElementTypeInfo for this type.
    getElementType :: Proxy a -> ElementTypeInfo

class BondType a => BondStruct a where
    -- | Read struct from untagged stream
    bondStructGetUntagged :: (Functor (ReaderM t), Monad (ReaderM t), Protocol t) => BondGet t a
    bondStructGetBase :: (Monad (ReaderM t), Protocol t) => a -> BondGet t a
    bondStructGetField :: (Functor (ReaderM t), Monad (ReaderM t), Protocol t) => Ordinal -> a -> BondGet t a
    -- | Put struct
    bondStructPut :: (Monad (BondPutM t), Protocol t) => a -> BondPut t
    getSchema :: Proxy a -> StructSchema

class Protocol t where
    type ReaderM t :: * -> *
    type WriterM t :: * -> *
    -- | encode top-level struct
    bondPutStruct :: BondStruct a => a -> BondPut t
    -- | encode base struct
    bondPutBaseStruct :: BondStruct a => a -> BondPut t
    -- | decode top-level struct
    bondGetStruct :: BondStruct a => BondGet t a
    -- | decode base struct
    bondGetBaseStruct :: BondStruct a => BondGet t a

    bondPutField :: (BondType a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut t
    bondPutDefNothingField :: (BondType a, BondStruct b) => Proxy b -> Ordinal -> Maybe a -> BondPut t

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
    bondPutList :: BondType a => [a] -> BondPut t
    bondPutVector :: BondType a => V.Vector a -> BondPut t
    bondPutHashSet :: BondType a => H.HashSet a -> BondPut t
    bondPutSet :: BondType a => S.Set a -> BondPut t
    bondPutMap :: (BondType k, BondType v) => M.Map k v -> BondPut t
    bondPutNullable :: BondType a => Maybe a -> BondPut t
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
    bondGetList :: BondType a => BondGet t [a]
    bondGetVector :: BondType a => BondGet t (V.Vector a)
    bondGetHashSet :: (Eq a, Hashable a, BondType a) => BondGet t (H.HashSet a)
    bondGetSet :: (Ord a, BondType a) => BondGet t (S.Set a)
    bondGetMap :: (Ord k, BondType k, BondType v) => BondGet t (M.Map k v)
    bondGetNullable :: BondType a => BondGet t (Maybe a)
    bondGetDefNothing :: BondType a => BondGet t (Maybe a)
    bondGetBonded :: BondStruct a => BondGet t (Bonded a)

instance BondType Float where
    bondGet = bondGetFloat
    bondPut = bondPutFloat
    getName _ = "float"
    getQualifiedName _ = "float"
    getElementType _ = ElementFloat

instance BondType Double where
    bondGet = bondGetDouble
    bondPut = bondPutDouble
    getName _ = "double"
    getQualifiedName _ = "double"
    getElementType _ = ElementDouble

instance BondType Bool where
    bondGet = bondGetBool
    bondPut = bondPutBool
    getName _ = "bool"
    getQualifiedName _ = "bool"
    getElementType _ = ElementBool

instance BondType Int8 where
    bondGet = bondGetInt8
    bondPut = bondPutInt8
    getName _ = "int8"
    getQualifiedName _ = "int8"
    getElementType _ = ElementInt8

instance BondType Int16 where
    bondGet = bondGetInt16
    bondPut = bondPutInt16
    getName _ = "int16"
    getQualifiedName _ = "int16"
    getElementType _ = ElementInt16

instance BondType Int32 where
    bondGet = bondGetInt32
    bondPut = bondPutInt32
    getName _ = "int32"
    getQualifiedName _ = "int32"
    getElementType _ = ElementInt32

instance BondType Int64 where
    bondGet = bondGetInt64
    bondPut = bondPutInt64
    getName _ = "int64"
    getQualifiedName _ = "int64"
    getElementType _ = ElementInt64

instance BondType Word8 where
    bondGet = bondGetUInt8
    bondPut = bondPutUInt8
    getName _ = "uint8"
    getQualifiedName _ = "uint8"
    getElementType _ = ElementUInt8

instance BondType Word16 where
    bondGet = bondGetUInt16
    bondPut = bondPutUInt16
    getName _ = "uint16"
    getQualifiedName _ = "uint16"
    getElementType _ = ElementUInt16

instance BondType Word32 where
    bondGet = bondGetUInt32
    bondPut = bondPutUInt32
    getName _ = "uint32"
    getQualifiedName _ = "uint32"
    getElementType _ = ElementUInt32

instance BondType Word64 where
    bondGet = bondGetUInt64
    bondPut = bondPutUInt64
    getName _ = "uint64"
    getQualifiedName _ = "uint64"
    getElementType _ = ElementUInt64

instance BondType Utf8 where
    bondGet = bondGetString
    bondPut = bondPutString
    getName _ = "string"
    getQualifiedName _ = "string"
    getElementType _ = ElementString

instance BondType Utf16 where
    bondGet = bondGetWString
    bondPut = bondPutWString
    getName _ = "wstring"
    getQualifiedName _ = "wstring"
    getElementType _ = ElementWString

instance BondType Blob where
    bondGet = bondGetBlob
    bondPut = bondPutBlob
    getName _ = "blob"
    getQualifiedName _ = "blob"
    getElementType _ = ElementList ElementInt8

instance BondType a => BondType [a] where
    bondGet = bondGetList
    bondPut = bondPutList
    getName _ = makeGenericName "list" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "list" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementList $ getElementType (Proxy :: Proxy a)

instance BondType a => BondType (V.Vector a) where
    bondGet = bondGetVector
    bondPut = bondPutVector
    getName _ = makeGenericName "vector" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "vector" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementList $ getElementType (Proxy :: Proxy a)

instance (Eq a, Hashable a, BondType a) => BondType (H.HashSet a) where
    bondGet = bondGetHashSet
    bondPut = bondPutHashSet
    getName _ = makeGenericName "set" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "set" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementSet $ getElementType (Proxy :: Proxy a)

instance (Ord a, BondType a) => BondType (S.Set a) where
    bondGet = bondGetSet
    bondPut = bondPutSet
    getName _ = makeGenericName "set" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "set" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementSet $ getElementType (Proxy :: Proxy a)

instance (Ord k, BondType k, BondType v) => BondType (M.Map k v) where
    bondGet = bondGetMap
    bondPut = bondPutMap
    getName _ = makeGenericName "map" [getName (Proxy :: Proxy k), getName (Proxy :: Proxy v)]
    getQualifiedName _ = makeGenericName "map"
                            [ getQualifiedName (Proxy :: Proxy k)
                            , getQualifiedName (Proxy :: Proxy v)
                            ]
    getElementType _ = ElementMap (getElementType (Proxy :: Proxy k)) (getElementType (Proxy :: Proxy v))

instance BondStruct a => BondType (Bonded a) where
    bondGet = bondGetBonded
    bondPut = bondPutBonded
    getName _ = makeGenericName "bonded" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "bonded" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementBonded $ getSchema (Proxy :: Proxy a)

instance BondType a => BondType (Maybe a) where
    bondGet = bondGetNullable
    bondPut = bondPutNullable
    getName _ = makeGenericName "nullable" [getName (Proxy :: Proxy a)]
    getQualifiedName _ = makeGenericName "nullable" [getQualifiedName (Proxy :: Proxy a)]
    getElementType _ = ElementList $ getElementType (Proxy :: Proxy a)
