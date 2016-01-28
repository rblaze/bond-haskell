{-# Language ScopedTypeVariables #-}
module Data.Bond.Schema (
    FieldDef(..),
    Metadata(..),
    SchemaState,
    TypeDefGen(..),
    Variant(..),
    findTypeDef,
    getSchema,
    makeFieldDef,
    makeGenericTypeName,
    makeStructMeta,
    optional,
    required,
    requiredOptional,
    withStruct
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
import Data.List
import Data.Map.Strict ((!))
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
    getTypeName :: Proxy a -> String
    getQualifiedTypeName :: Proxy a -> String

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

makeFieldDef :: BondStruct a => Proxy a -> Word16 -> TypeDef -> FieldDef
makeFieldDef p n = FieldDef meta n
    where
    info = fieldsInfo p ! Ordinal n
    meta = defaultValue
        { name = fromText $ fieldName info
        , attributes = M.fromList $ map (fromText *** fromText) $ M.toList $ fieldAttrs info
        , modifier = fieldModifier info
        , default_value = fieldDefault info
        }

-- | one day I would drop support for base < 4.7 (ghc 7.6) and get rid of this function
typeRep' :: Typeable a => Proxy a -> TypeRep
typeRep' p = typeOf (undefined `asProxyTypeOf` p)

withStruct :: Typeable a => Proxy a -> SchemaMonad (Metadata, Maybe TypeDef, [FieldDef]) -> SchemaState
withStruct p makeStructDef = do
    -- put a placeholder into struct table, otherwise recursive definitions will loop
    (known, ss) <- get
    let idx = S.length ss
    let td = defaultValue { struct_def = fromIntegral idx }
    put (M.insert (typeRep' p) td known, ss |> error "SchemaDef placeholder value")
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

makeGenericTypeName :: String -> [String] -> String
makeGenericTypeName s params = s ++ '<' : intercalate "," params ++ ">"

instance TypeDefGen Bool where
    getTypeDef _ = simpleType bT_BOOL
    getTypeName _ = "bool"
    getQualifiedTypeName _ = "bool"

instance TypeDefGen Int8 where
    getTypeDef _ = simpleType bT_INT8
    getTypeName _ = "int8"
    getQualifiedTypeName _ = "int8"

instance TypeDefGen Int16 where
    getTypeDef _ = simpleType bT_INT16
    getTypeName _ = "int16"
    getQualifiedTypeName _ = "int16"

instance TypeDefGen Int32 where
    getTypeDef _ = simpleType bT_INT32
    getTypeName _ = "int32"
    getQualifiedTypeName _ = "int32"

instance TypeDefGen Int64 where
    getTypeDef _ = simpleType bT_INT64
    getTypeName _ = "int64"
    getQualifiedTypeName _ = "int64"

instance TypeDefGen Word8 where
    getTypeDef _ = simpleType bT_UINT8
    getTypeName _ = "uint8"
    getQualifiedTypeName _ = "uint8"

instance TypeDefGen Word16 where
    getTypeDef _ = simpleType bT_UINT16
    getTypeName _ = "uint16"
    getQualifiedTypeName _ = "uint16"

instance TypeDefGen Word32 where
    getTypeDef _ = simpleType bT_UINT32
    getTypeName _ = "uint32"
    getQualifiedTypeName _ = "uint32"

instance TypeDefGen Word64 where
    getTypeDef _ = simpleType bT_UINT64
    getTypeName _ = "uint64"
    getQualifiedTypeName _ = "uint64"

instance TypeDefGen Float where
    getTypeDef _ = simpleType bT_FLOAT
    getTypeName _ = "float"
    getQualifiedTypeName _ = "float"

instance TypeDefGen Double where
    getTypeDef _ = simpleType bT_DOUBLE
    getTypeName _ = "double"
    getQualifiedTypeName _ = "double"

instance TypeDefGen Utf8 where
    getTypeDef _ = simpleType bT_STRING
    getTypeName _ = "string"
    getQualifiedTypeName _ = "string"

instance TypeDefGen Utf16 where
    getTypeDef _ = simpleType bT_WSTRING
    getTypeName _ = "wstring"
    getQualifiedTypeName _ = "wstring"

instance TypeDefGen Blob where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy Int8)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }
    getTypeName _ = "blob"
    getQualifiedTypeName _ = "blob"

instance TypeDefGen t => TypeDefGen (Bonded t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ td {
            bonded_type = True
        }
    getTypeName _ = "bonded<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "bonded<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance TypeDefGen t => TypeDefGen (Maybe t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }
    getTypeName _ = "nullable<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "nullable<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance TypeDefGen t => TypeDefGen [t] where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }
    getTypeName _ = "list<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "list<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance TypeDefGen t => TypeDefGen (Vector t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_LIST,
            element = Just td
        }
    getTypeName _ = "vector<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "vector<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance TypeDefGen t => TypeDefGen (HashSet t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_SET,
            element = Just td
        }
    getTypeName _ = "set<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "set<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance TypeDefGen t => TypeDefGen (Set t) where
    getTypeDef _ = do
        td <- findTypeDef (Proxy :: Proxy t)
        return $ defaultValue {
            TD.id = bT_SET,
            element = Just td
        }
    getTypeName _ = "set<" ++ getTypeName (Proxy :: Proxy t) ++ ">"
    getQualifiedTypeName _ = "set<" ++ getQualifiedTypeName (Proxy :: Proxy t) ++ ">"

instance (TypeDefGen k, TypeDefGen v) => TypeDefGen (Map k v) where
    getTypeDef _ = do
        tk <- findTypeDef (Proxy :: Proxy k)
        tv <- findTypeDef (Proxy :: Proxy v)
        return $ defaultValue {
            TD.id = bT_MAP,
            element = Just tv,
            key = Just tk
        }
    getTypeName _ = "map<" ++ getTypeName (Proxy :: Proxy k) ++ ',' : getTypeName (Proxy :: Proxy v) ++ ">"
    getQualifiedTypeName _ = "map<" ++ getQualifiedTypeName (Proxy :: Proxy k)
            ++ ',' : getQualifiedTypeName (Proxy :: Proxy v) ++ ">"
