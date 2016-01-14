{-# Language ScopedTypeVariables #-}
module Data.Bond.Schema (
    Metadata(..),
    FieldDef(..),
    Variant(..),
    optional,
    required,
    requiredOptional,
    SchemaState,
    TypeDefGen(..),
    makeFieldDef,
    makeStructMeta,
    makeFieldMeta,
    makeFieldMetaWithDef,
    withStruct,
    findTypeDef,
    getSchema
  ) where

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.FieldDef
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.Variant
import Data.Bond.Schema.StructDef as SD
import Data.Bond.Schema.TypeDef as TD

import Data.Bond.Default
import Data.Bond.Proto
import Data.Bond.Types

import Control.Arrow
import Control.Monad.State
import Data.Foldable
import Data.Proxy
import Data.Sequence ((|>))
import Data.Typeable
import qualified Data.Sequence as S
import qualified Data.Map as ML
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type SchemaMonad = State (M.Map TypeRep TypeDef, S.Seq StructDef)
type SchemaState = SchemaMonad TypeDef

class Typeable a => TypeDefGen a where
    getTypeDef :: Proxy a -> SchemaState

getSchema :: BondStruct a => Proxy a -> SchemaDef
getSchema p = let (t, (_, ss)) = runState (getTypeDef p) (M.empty, S.empty)
               in SchemaDef { structs = V.fromList $ toList ss, root = t }

-- dumb wrappers for breaking module cycles
makeStructMeta :: String -> String -> [(String, String)] -> Metadata
makeStructMeta mname mqname mattrs = defaultValue {
    name = fromString mname,
    qualified_name = fromString mqname,
    attributes = ML.fromList $ map (fromString *** fromString) mattrs
  }

makeFieldMeta :: String -> [(String, String)] -> Modifier -> Metadata
makeFieldMeta mname mattrs mmod = defaultValue {
    name = fromString mname,
    attributes = ML.fromList $ map (fromString *** fromString) mattrs,
    modifier = mmod
  }

makeFieldMetaWithDef :: Variant -> String -> [(String, String)] -> Modifier -> Metadata
makeFieldMetaWithDef defvalue mname mattrs mmod = defaultValue {
    name = fromString mname,
    attributes = ML.fromList $ map (fromString *** fromString) mattrs,
    modifier = mmod,
    default_value = defvalue
  }

makeFieldDef :: Metadata -> Word16 -> TypeDef -> FieldDef
makeFieldDef = FieldDef

-- | one day I would drop support for base < 4.7 (ghc 7.6) and get rid of this function
typeRep' :: Typeable a => Proxy a -> TypeRep
typeRep' p = typeOf (undefined `asProxyTypeOf` p)

withStruct :: Typeable a => SchemaMonad (Metadata, Maybe TypeDef, [FieldDef]) -> Proxy a -> SchemaState
withStruct makeStructDef p = do
    -- put a placeholder into struct table, otherwise recursive definitions will loop
    (known, ss) <- get
    let idx = S.length ss
    let td = defaultValue { struct_def = fromIntegral idx }
    put (M.insert (typeRep' p) td known, ss |> error "unpublished schema")
    -- make struct def
    -- return tuple to avoid name clash on metadata in generated code
    (structMeta, structBase, structFields) <- makeStructDef
    -- put real definition to array
    modify $ second (S.update idx 
        defaultValue{SD.metadata = structMeta, SD.base_def = structBase, SD.fields = V.fromList structFields}
      )
    return td

findTypeDef :: TypeDefGen a => Proxy a -> SchemaState
findTypeDef p = do
    (known, _) <- get
    let typekey = typeRep' p
    let val = M.lookup typekey known
    case val of
        Just t -> return t
        Nothing -> getTypeDef p

simpleType :: BondDataType -> SchemaState
simpleType t = return $ defaultValue { TD.id = t }

instance TypeDefGen Bool where getTypeDef _ = simpleType bT_BOOL
instance TypeDefGen Int8 where getTypeDef _ = simpleType bT_INT8
instance TypeDefGen Int16 where getTypeDef _ = simpleType bT_INT16
instance TypeDefGen Int32 where getTypeDef _ = simpleType bT_INT32
instance TypeDefGen Int64 where getTypeDef _ = simpleType bT_INT64
instance TypeDefGen Word8 where getTypeDef _ = simpleType bT_UINT8
instance TypeDefGen Word16 where getTypeDef _ = simpleType bT_UINT16
instance TypeDefGen Word32 where getTypeDef _ = simpleType bT_UINT32
instance TypeDefGen Word64 where getTypeDef _ = simpleType bT_UINT64
instance TypeDefGen Float where getTypeDef _ = simpleType bT_FLOAT
instance TypeDefGen Double where getTypeDef _ = simpleType bT_DOUBLE
instance TypeDefGen Utf8 where getTypeDef _ = simpleType bT_STRING
instance TypeDefGen Utf16 where getTypeDef _ = simpleType bT_WSTRING

instance TypeDefGen Blob where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy Word8)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }

instance TypeDefGen t => TypeDefGen (Bonded t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_STRUCT,
            element = Just td,
            bonded_type = True
        }

instance TypeDefGen t => TypeDefGen (Maybe t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }

instance TypeDefGen t => TypeDefGen [t] where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }

instance TypeDefGen t => TypeDefGen (Vector t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }

instance TypeDefGen t => TypeDefGen (HashSet t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_SET,
            element = Just td
        }

instance TypeDefGen t => TypeDefGen (Set t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_SET,
            element = Just td
        }

instance (TypeDefGen k, TypeDefGen v) => TypeDefGen (Map k v) where
    getTypeDef _ = do
        tk <- findTypeDef (Proxy :: Proxy k)
        tv <- findTypeDef (Proxy :: Proxy v)
        return $ defaultValue {
            TD.id = bT_MAP,
            element = Just tv,
            key = Just tk
        }
