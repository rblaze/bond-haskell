module Data.Bond.Internal.SchemaOps where

import Data.Bond.Default
import Data.Bond.Types
import Data.Bond.Internal.OrdinalSet
import Data.Bond.Internal.TypedSchema
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.Variant
import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.StructDef as SD
import qualified Data.Bond.Schema.TypeDef as TD

import Control.Arrow
import Control.Monad.State.Strict
import Data.Maybe
import Data.Typeable
import Data.Vector ((//))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

parseSchema :: SchemaDef -> Either String StructSchema
parseSchema schemadef = validateSchemaDef >> makeSchema
    where
    validateSchemaDef = Right ()
    substructs = V.map compileStruct (structs schemadef)
    makeSchema = V.indexM substructs (fromIntegral $ TD.struct_def $ root schemadef)
    compileStruct struct =
        let meta = SD.metadata struct
            tycon = mkTyCon3 "Bond" "RuntimeSchema" (toString $ qualified_name meta)
            typerep = mkTyConApp tycon []
            mustWrite = fromOrdinalVector $ V.map (Ordinal . fromIntegral . FD.id) $
                V.filter (\ f -> modifier (FD.metadata f) /= optional) $ SD.fields struct
            mustRead = fromOrdinalVector $ V.map (Ordinal . fromIntegral . FD.id) $
                V.filter (\ f -> modifier (FD.metadata f) == required) $ SD.fields struct
            fields = M.fromList $ V.toList $ V.map makeField $ SD.fields struct
         in StructSchema
            { structTag = typerep
            , structName = toText (name meta)
            , structQualifiedName = toText (qualified_name meta)
            , structAttrs = M.fromList $ map (toText *** toText) $ M.toList $ attributes meta
            , structBase = fmap (V.unsafeIndex substructs . fromIntegral . TD.struct_def) (SD.base_def struct)
            , structFields = fields
            , structOrdinalsRequiredOnWrite = mustWrite
            , structOrdinalsRequiredOnRead = mustRead
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
                base <- case structBase struct of
                    Nothing -> return Nothing
                    Just s -> Just <$> makeStructDef s
                fieldVec <- fmap V.fromList $ mapM makeFieldDef $ M.toAscList $ structFields struct
                let structDef = SD.StructDef
                        { SD.metadata = defaultValue
                            { name = fromText (structName struct)
                            , qualified_name = fromText (structQualifiedName struct)
                            , attributes = M.fromList $ map (fromText *** fromText) $ M.toList $ structAttrs struct
                            }
                        , SD.base_def = base
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
