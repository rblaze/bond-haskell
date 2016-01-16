{-# Language ScopedTypeVariables #-}
module Data.Bond.Schema (
    Metadata(..),
    FieldDef(..),
    SchemaState,
    TypeDefGen(..),
    Variant(..),
    findTypeDef,
    makeFieldDef,
    makeFieldMeta,
    makeFieldMetaWithDef,
    makeGenericTypeName,
    makeStructMeta,
    optional,
    withStruct
  ) where

import {-# SOURCE #-} Data.Bond.Schema.FieldDef
import {-# SOURCE #-} Data.Bond.Schema.Metadata
import {-# SOURCE #-} Data.Bond.Schema.Modifier
import {-# SOURCE #-} Data.Bond.Schema.StructDef
import {-# SOURCE #-} Data.Bond.Schema.TypeDef
import {-# SOURCE #-} Data.Bond.Schema.Variant

import Data.Bond.Types
import Data.Proxy
import Data.Typeable
import Control.Monad.State
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M

type SchemaMonad = State (M.Map TypeRep TypeDef, S.Seq StructDef)
type SchemaState = SchemaMonad TypeDef

class Typeable a => TypeDefGen a where
    getTypeDef :: Proxy a -> SchemaState
    getTypeName :: Proxy a -> String
    getQualifiedTypeName :: Proxy a -> String

makeStructMeta :: String -> String -> [(String, String)] -> Metadata
makeFieldMeta :: String -> [(String, String)] -> Modifier -> Metadata
makeFieldMetaWithDef :: Variant -> String -> [(String, String)] -> Modifier -> Metadata
makeFieldDef :: Metadata -> Word16 -> TypeDef -> FieldDef
withStruct :: Typeable a => Proxy a -> SchemaMonad (Metadata, Maybe TypeDef, [FieldDef]) -> SchemaState
findTypeDef :: TypeDefGen a => Proxy a -> SchemaState
makeGenericTypeName :: String -> [String] -> String

instance TypeDefGen Bool
instance TypeDefGen Int8
instance TypeDefGen Int16
instance TypeDefGen Int32
instance TypeDefGen Int64
instance TypeDefGen Word8
instance TypeDefGen Word16
instance TypeDefGen Word32
instance TypeDefGen Word64
instance TypeDefGen Float
instance TypeDefGen Double
instance TypeDefGen Utf8
instance TypeDefGen Utf16
instance TypeDefGen Blob
instance TypeDefGen t => TypeDefGen (Bonded t)
instance TypeDefGen t => TypeDefGen (Maybe t)
instance TypeDefGen t => TypeDefGen [t]
instance TypeDefGen t => TypeDefGen (Vector t)
instance TypeDefGen t => TypeDefGen (HashSet t)
instance TypeDefGen t => TypeDefGen (Set t)
instance (TypeDefGen k, TypeDefGen v) => TypeDefGen (Map k v)
