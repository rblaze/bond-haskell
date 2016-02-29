{-# LANGUAGE FlexibleContexts #-}
module Data.Bond.Internal.SchemaOps where

import Data.Bond.Struct
import Data.Bond.TypedSchema
import Data.Bond.Types
import Data.Bond.Internal.Default
import Data.Bond.Internal.OrdinalSet
import Data.Bond.Internal.SchemaUtils
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.Variant
import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.StructDef as SD
import qualified Data.Bond.Schema.TypeDef as TD

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Error
import Data.Either
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Vector ((//))
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

validateSchemaDef :: MonadError String m => SchemaDef -> m ()
validateSchemaDef schema = do
    checkChain IS.empty 0
    let rootTD = root schema
    when (TD.id rootTD /= bT_STRUCT) $ throwError "root type is not struct"
    checkType rootTD
    V.mapM_ checkStruct (structs schema)
    where
    checkChain _ n | n == V.length (structs schema) = return ()
    checkChain seen n | IS.member n seen = checkChain seen (n + 1)
    checkChain seen n = do
        let step stack i = do
                when (i >= V.length (structs schema)) $ throwError $ "struct index " ++ show i ++ " out of range"
                when (IS.member i stack) $ throwError "loop in inheritance chain"
                let baseStruct = SD.base_def $ structs schema V.! i
                let newStack = IS.insert i stack
                case baseStruct of
                    Nothing -> return newStack
                    Just b -> do
                        when (TD.id b /= bT_STRUCT) $ throwError "not a struct in inheritance chain"
                        step newStack (fromIntegral $ TD.struct_def b)
        stack <- step IS.empty n
        checkChain (IS.union seen stack) (n + 1)
    checkStruct struct = do
        maybe (return ()) checkType (SD.base_def struct)
        -- FIXME check for duplicate ordinals
        V.forM_ (SD.fields struct) $ checkType . FD.typedef
    checkType t@TD.TypeDef{TD.id = typ}
        | typ == bT_BOOL = return ()
        | typ == bT_INT8 = return ()
        | typ == bT_INT16 = return ()
        | typ == bT_INT32 = return ()
        | typ == bT_INT64 = return ()
        | typ == bT_UINT8 = return ()
        | typ == bT_UINT16 = return ()
        | typ == bT_UINT32 = return ()
        | typ == bT_UINT64 = return ()
        | typ == bT_FLOAT = return ()
        | typ == bT_DOUBLE = return ()
        | typ == bT_STRING = return ()
        | typ == bT_WSTRING = return ()
        | typ == bT_LIST =
            case TD.element t of
                Nothing -> throwError "element type missing in list schema"
                Just subtype -> checkType subtype
        | typ == bT_SET =
            case TD.element t of
                Nothing -> throwError "element type missing in set schema"
                Just subtype -> checkType subtype
        | typ == bT_MAP = do
            case TD.element t of
                Nothing -> throwError "value type missing in map schema"
                Just subtype -> checkType subtype
            case TD.key t of
                Nothing -> throwError "key type missing in map schema"
                Just subtype -> checkType subtype
        | typ == bT_STRUCT = do
            let idx = fromIntegral $ TD.struct_def t
            when (idx >= V.length (structs schema)) $ throwError $ "struct index " ++ show idx ++ " out of range"
        | otherwise = throwError $ "unexpected data type " ++ bondTypeName typ

parseSchema :: SchemaDef -> Either String StructSchema
parseSchema schemadef = validateSchemaDef schemadef >> makeSchema
    where
    substructs = V.map compileStruct (structs schemadef)
    makeSchema = V.indexM substructs (fromIntegral $ TD.struct_def $ root schemadef)
    compileStruct struct =
        let meta = SD.metadata struct
            tycon = mkTyCon3 "Bond" "RuntimeSchema" (toString $ qualified_name meta)
            typerep = mkTyConApp tycon []
            requiredOrdinals = fromOrdinalVector $ V.map (Ordinal . fromIntegral . FD.id) $
                V.filter (\ f -> modifier (FD.metadata f) == required) $ SD.fields struct
            fieldMap = M.fromList $ V.toList $ V.map makeField $ SD.fields struct
         in StructSchema
            { structTag = typerep
            , structName = toText (name meta)
            , structQualifiedName = toText (qualified_name meta)
            , structAttrs = M.fromList $ map (toText *** toText) $ M.toList $ attributes meta
            , structBase = fmap (V.unsafeIndex substructs . fromIntegral . TD.struct_def) (SD.base_def struct)
            , structFields = fieldMap
            , structRequiredOrdinals = requiredOrdinals
            }
    makeField field =
        let meta = FD.metadata field
            fieldMod
                | modifier meta == optional = FieldOptional
                | modifier meta == required = FieldRequired
                | otherwise = FieldRequiredOptional
            schema = FieldSchema
                { fieldName = toText (name meta)
                , fieldAttrs = M.fromList $ map (toText *** toText) $ M.toList $ attributes meta
                , fieldModifier = fieldMod
                , fieldType = makeFieldType (FD.typedef field) (default_value $ FD.metadata field)
                }
         in (Ordinal $ FD.id field, schema)
    makeFieldType td variant
        | TD.id td == bT_BOOL = FieldBool $ defnothing (uint_value variant /= 0)
        | TD.id td == bT_INT8 = FieldInt8 $ defnothing (fromIntegral $ int_value variant)
        | TD.id td == bT_INT16 = FieldInt16 $ defnothing (fromIntegral $ int_value variant)
        | TD.id td == bT_INT32 = FieldInt32 $ defnothing (fromIntegral $ int_value variant)
        | TD.id td == bT_INT64 = FieldInt64 $ defnothing (int_value variant)
        | TD.id td == bT_UINT8 = FieldUInt8 $ defnothing (fromIntegral $ uint_value variant)
        | TD.id td == bT_UINT16 = FieldUInt16 $ defnothing (fromIntegral $ uint_value variant)
        | TD.id td == bT_UINT32 = FieldUInt32 $ defnothing (fromIntegral $ uint_value variant)
        | TD.id td == bT_UINT64 = FieldUInt64 $ defnothing (uint_value variant)
        | TD.id td == bT_FLOAT = FieldFloat $ defnothing (realToFrac $ double_value variant)
        | TD.id td == bT_DOUBLE = FieldDouble $ defnothing (double_value variant)
        | TD.id td == bT_STRING = FieldString $ defnothing (string_value variant)
        | TD.id td == bT_WSTRING = FieldWString $ defnothing (wstring_value variant)
        | TD.id td == bT_STRUCT && TD.bonded_type td = FieldBonded (defnothing ()) (V.unsafeIndex substructs $ fromIntegral $ TD.struct_def td)
        | TD.id td == bT_STRUCT = FieldStruct (defnothing ()) (V.unsafeIndex substructs $ fromIntegral $ TD.struct_def td)
        | TD.id td == bT_LIST = FieldList (defnothing ()) (makeElementType $ fromJust $ TD.element td)
        | TD.id td == bT_SET = FieldSet (defnothing ()) (makeElementType $ fromJust $ TD.element td)
        | TD.id td == bT_MAP = FieldMap (defnothing ()) (makeElementType $ fromJust $ TD.key td) (makeElementType $ fromJust $ TD.element td)
        | otherwise = error $ "internal error: schema validation missed invalid type tag " ++ show (TD.id td)
        where
        defnothing v = if nothing variant then DefaultNothing else DefaultValue v
    makeElementType td
        | TD.id td == bT_BOOL = ElementBool
        | TD.id td == bT_INT8 = ElementInt8
        | TD.id td == bT_INT16 = ElementInt16
        | TD.id td == bT_INT32 = ElementInt32
        | TD.id td == bT_INT64 = ElementInt64
        | TD.id td == bT_UINT8 = ElementUInt8
        | TD.id td == bT_UINT16 = ElementUInt16
        | TD.id td == bT_UINT32 = ElementUInt32
        | TD.id td == bT_UINT64 = ElementUInt64
        | TD.id td == bT_FLOAT = ElementFloat
        | TD.id td == bT_DOUBLE = ElementDouble
        | TD.id td == bT_STRING = ElementString
        | TD.id td == bT_WSTRING = ElementWString
        | TD.id td == bT_STRUCT && TD.bonded_type td = ElementBonded (V.unsafeIndex substructs $ fromIntegral $ TD.struct_def td)
        | TD.id td == bT_STRUCT = ElementStruct (V.unsafeIndex substructs $ fromIntegral $ TD.struct_def td)
        | TD.id td == bT_LIST = ElementList (makeElementType $ fromJust $ TD.element td)
        | TD.id td == bT_SET = ElementSet (makeElementType $ fromJust $ TD.element td)
        | TD.id td == bT_MAP = ElementMap (makeElementType $ fromJust $ TD.key td) (makeElementType $ fromJust $ TD.element td)
        | otherwise = error $ "internal error: schema validation missed invalid type tag " ++ show (TD.id td)

data SchemaState = SchemaState
    { knownStructs :: V.Vector SD.StructDef
    , structMap :: M.Map TypeRep Word16
    }

assembleSchema :: StructSchema -> SchemaDef
assembleSchema schema = SchemaDef { structs = structVector, root = rootStruct }
    where
    (rootStruct, SchemaState{knownStructs = structVector}) = runState (makeStructDef schema) (SchemaState V.empty M.empty)
    makeStructDef :: StructSchema -> State SchemaState TD.TypeDef
    makeStructDef struct = do
        m <- gets structMap
        idx <- case M.lookup (structTag struct) m of
            Just i -> return i
            Nothing -> do
                vec <- gets knownStructs
                let i = V.length vec
                let vnew = V.snoc vec (error "internal error: unfinished StructDef used")
                put $ SchemaState vnew (M.insert (structTag struct) (fromIntegral i) m)
                baseTypeDef <- case structBase struct of
                    Nothing -> return Nothing
                    Just s -> Just <$> makeStructDef s
                fieldVec <- fmap V.fromList $ mapM makeFieldDef $ M.toAscList $ structFields struct
                let structDef = SD.StructDef
                        { SD.metadata = defaultValue
                            { name = fromText (structName struct)
                            , qualified_name = fromText (structQualifiedName struct)
                            , attributes = M.fromList $ map (fromText *** fromText) $ M.toList $ structAttrs struct
                            }
                        , SD.base_def = baseTypeDef
                        , SD.fields = fieldVec
                        }
                modify $ \ s -> let bigvec = knownStructs s
                                 in s{ knownStructs = bigvec // [(i, structDef)] }
                return (fromIntegral i)
        return defaultValue{ TD.struct_def = idx }
    makeFieldDef (Ordinal n, field) = do
        fieldTypeDef <- makeFieldTypeDef (fieldType field)
        return defaultValue
            { FD.metadata = defaultValue
                { name = fromText (fieldName field)
                , attributes = M.fromList $ map (fromText *** fromText) $ M.toList $ fieldAttrs field
                , modifier = case fieldModifier field of
                    FieldOptional -> optional
                    FieldRequired -> required
                    FieldRequiredOptional -> requiredOptional
                , default_value = makeDefaultValue (fieldType field)
                }
            , FD.id = n
            , FD.typedef = fieldTypeDef
            }

    makeFieldTypeDef (FieldBool _) = return defaultValue{TD.id = bT_BOOL}
    makeFieldTypeDef (FieldInt8 _) = return defaultValue{TD.id = bT_INT8}
    makeFieldTypeDef (FieldInt16 _) = return defaultValue{TD.id = bT_INT16}
    makeFieldTypeDef (FieldInt32 _) = return defaultValue{TD.id = bT_INT32}
    makeFieldTypeDef (FieldInt64 _) = return defaultValue{TD.id = bT_INT64}
    makeFieldTypeDef (FieldUInt8 _) = return defaultValue{TD.id = bT_UINT8}
    makeFieldTypeDef (FieldUInt16 _) = return defaultValue{TD.id = bT_UINT16}
    makeFieldTypeDef (FieldUInt32 _) = return defaultValue{TD.id = bT_UINT32}
    makeFieldTypeDef (FieldUInt64 _) = return defaultValue{TD.id = bT_UINT64}
    makeFieldTypeDef (FieldFloat _) = return defaultValue{TD.id = bT_FLOAT}
    makeFieldTypeDef (FieldDouble _) = return defaultValue{TD.id = bT_DOUBLE}
    makeFieldTypeDef (FieldString _) = return defaultValue{TD.id = bT_STRING}
    makeFieldTypeDef (FieldWString _) = return defaultValue{TD.id = bT_WSTRING}
    makeFieldTypeDef (FieldStruct _ substruct) = makeStructDef substruct
    makeFieldTypeDef (FieldBonded _ substruct) = do
        typeDef <- makeStructDef substruct
        return typeDef{TD.bonded_type = True}
    makeFieldTypeDef (FieldList _ element) = do
        typeDef <- makeElementTypeDef element
        return defaultValue{TD.id = bT_LIST, TD.element = Just typeDef}
    makeFieldTypeDef (FieldSet _ element) = do
        typeDef <- makeElementTypeDef element
        return defaultValue{TD.id = bT_SET, TD.element = Just typeDef}
    makeFieldTypeDef (FieldMap _ key value) = do
        keyTypeDef <- makeElementTypeDef key
        valueTypeDef <- makeElementTypeDef value
        return defaultValue
            { TD.id = bT_MAP
            , TD.element = Just valueTypeDef
            , TD.key = Just keyTypeDef
            }

    makeElementTypeDef ElementBool = return defaultValue{TD.id = bT_BOOL}
    makeElementTypeDef ElementInt8 = return defaultValue{TD.id = bT_INT8}
    makeElementTypeDef ElementInt16 = return defaultValue{TD.id = bT_INT16}
    makeElementTypeDef ElementInt32 = return defaultValue{TD.id = bT_INT32}
    makeElementTypeDef ElementInt64 = return defaultValue{TD.id = bT_INT64}
    makeElementTypeDef ElementUInt8 = return defaultValue{TD.id = bT_UINT8}
    makeElementTypeDef ElementUInt16 = return defaultValue{TD.id = bT_UINT16}
    makeElementTypeDef ElementUInt32 = return defaultValue{TD.id = bT_UINT32}
    makeElementTypeDef ElementUInt64 = return defaultValue{TD.id = bT_UINT64}
    makeElementTypeDef ElementFloat = return defaultValue{TD.id = bT_FLOAT}
    makeElementTypeDef ElementDouble = return defaultValue{TD.id = bT_DOUBLE}
    makeElementTypeDef ElementString = return defaultValue{TD.id = bT_STRING}
    makeElementTypeDef ElementWString = return defaultValue{TD.id = bT_WSTRING}
    makeElementTypeDef (ElementStruct substruct) = makeStructDef substruct
    makeElementTypeDef (ElementBonded substruct) = do
        typeDef <- makeStructDef substruct
        return typeDef{TD.bonded_type = True}
    makeElementTypeDef (ElementList element) = do
        typeDef <- makeElementTypeDef element
        return defaultValue{TD.id = bT_LIST, TD.element = Just typeDef}
    makeElementTypeDef (ElementSet element) = do
        typeDef <- makeElementTypeDef element
        return defaultValue{TD.id = bT_SET, TD.element = Just typeDef}
    makeElementTypeDef (ElementMap key value) = do
        keyTypeDef <- makeElementTypeDef key
        valueTypeDef <- makeElementTypeDef value
        return defaultValue
            { TD.id = bT_MAP
            , TD.element = Just valueTypeDef
            , TD.key = Just keyTypeDef
            }

    makeDefaultValue (FieldBool DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldInt8 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldInt16 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldInt32 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldInt64 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldUInt8 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldUInt16 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldUInt32 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldUInt64 DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldFloat DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldDouble DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldString DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldWString DefaultNothing) = defaultValue{nothing = True}
    makeDefaultValue (FieldStruct DefaultNothing _) = defaultValue{nothing = True}
    makeDefaultValue (FieldBonded DefaultNothing _) = defaultValue{nothing = True}
    makeDefaultValue (FieldList DefaultNothing _) = defaultValue{nothing = True}
    makeDefaultValue (FieldSet DefaultNothing _) = defaultValue{nothing = True}
    makeDefaultValue (FieldMap DefaultNothing _ _) = defaultValue{nothing = True}
    makeDefaultValue (FieldBool (DefaultValue v)) = defaultValue{uint_value = if v then 1 else 0}
    makeDefaultValue (FieldInt8 (DefaultValue v)) = defaultValue{int_value = fromIntegral v}
    makeDefaultValue (FieldInt16 (DefaultValue v)) = defaultValue{int_value = fromIntegral v}
    makeDefaultValue (FieldInt32 (DefaultValue v)) = defaultValue{int_value = fromIntegral v}
    makeDefaultValue (FieldInt64 (DefaultValue v)) = defaultValue{int_value = v}
    makeDefaultValue (FieldUInt8 (DefaultValue v)) = defaultValue{uint_value = fromIntegral v}
    makeDefaultValue (FieldUInt16 (DefaultValue v)) = defaultValue{uint_value = fromIntegral v}
    makeDefaultValue (FieldUInt32 (DefaultValue v)) = defaultValue{uint_value = fromIntegral v}
    makeDefaultValue (FieldUInt64 (DefaultValue v)) = defaultValue{uint_value = v}
    makeDefaultValue (FieldFloat (DefaultValue v)) = defaultValue{double_value = realToFrac v}
    makeDefaultValue (FieldDouble (DefaultValue v)) = defaultValue{double_value = v}
    makeDefaultValue (FieldString (DefaultValue v)) = defaultValue{string_value = v}
    makeDefaultValue (FieldWString (DefaultValue v)) = defaultValue{wstring_value = v}
    makeDefaultValue (FieldStruct (DefaultValue ()) _) = defaultValue
    makeDefaultValue (FieldBonded (DefaultValue ()) _) = defaultValue
    makeDefaultValue (FieldList (DefaultValue ()) _) = defaultValue
    makeDefaultValue (FieldSet (DefaultValue ()) _) = defaultValue
    makeDefaultValue (FieldMap (DefaultValue ()) _ _) = defaultValue

checkStructSchema :: MonadError String m => StructSchema -> Struct -> m Struct
checkStructSchema rootSchema rootStruct = do
    when (length schemaStack > length structStack) $ throwError "schema depth is larger than struct depth"
    let shortStructStack = take (length schemaStack) structStack
    let errs = lefts $ zipWith checkStackLevel schemaStack shortStructStack
    unless (null errs) $ throwError $ intercalate "\n" errs
    return $ head shortStructStack
    where
    checkStackLevel schema struct = mapM_ (checkField struct) (M.toList $ structFields schema)
    checkField struct (fieldId, fieldInfo) = case M.lookup fieldId (fields struct) of
        Nothing -> when (fieldModifier fieldInfo /= FieldOptional) $ Left $ "non-optional field " ++ show (fieldName fieldInfo) ++ " missing"
        Just v -> checkValueType (fieldToElementType $ fieldType fieldInfo) v
    checkValueType ElementBool (BOOL _) = Right ()
    checkValueType ElementInt8 (INT8 _) = Right ()
    checkValueType ElementInt16 (INT16 _) = Right ()
    checkValueType ElementInt32 (INT32 _) = Right ()
    checkValueType ElementInt64 (INT64 _) = Right ()
    checkValueType ElementUInt8 (UINT8 _) = Right ()
    checkValueType ElementUInt16 (UINT16 _) = Right ()
    checkValueType ElementUInt32 (UINT32 _) = Right ()
    checkValueType ElementUInt64 (UINT64 _) = Right ()
    checkValueType ElementFloat (FLOAT _) = Right ()
    checkValueType ElementDouble (DOUBLE _) = Right ()
    checkValueType ElementString (STRING _) = Right ()
    checkValueType ElementWString (WSTRING _) = Right ()
    checkValueType (ElementStruct schema) (STRUCT struct) = void $ checkStructSchema schema struct
    checkValueType (ElementBonded _) (BONDED _) = Right ()
    checkValueType (ElementBonded _) (STRUCT _) = Right ()
    checkValueType (ElementList element) (LIST bt xs) = do
        let expectedbt = elementToBondDataType element
        when (bt /= expectedbt) $ Left $ "list element type " ++ bondTypeName bt ++ " does not match schema type " ++ bondTypeName expectedbt
        mapM_ (checkValueType element) xs
    checkValueType (ElementSet element) (SET bt xs) = do
        let expectedbt = elementToBondDataType element
        when (bt /= expectedbt) $ Left $ "set element type " ++ bondTypeName bt ++ " does not match schema type " ++ bondTypeName expectedbt
        mapM_ (checkValueType element) xs
    checkValueType (ElementMap key value) (MAP btkey btvalue xs) = do
        let expectedbtkey = elementToBondDataType key
        let expectedbtvalue = elementToBondDataType value
        when (btkey /= expectedbtkey) $ Left $ "map key element type " ++ bondTypeName btkey ++ " does not match schema type " ++ bondTypeName expectedbtkey
        when (btvalue /= expectedbtvalue) $ Left $ "map value element type " ++ bondTypeName btvalue ++ " does not match schema type " ++ bondTypeName expectedbtvalue
        forM_ xs $ \(k, v) -> checkValueType key k >> checkValueType value v
    checkValueType t v = Left $ "field type " ++ valueName v ++ " does not match schema type " ++ bondTypeName (elementToBondDataType t)

    structStack = let step s = case base s of
                                Nothing -> [s]
                                Just b -> s : step b
                   in step rootStruct
    schemaStack = let step s = case structBase s of
                                Nothing -> [s]
                                Just b -> s : step b
                   in step rootSchema
