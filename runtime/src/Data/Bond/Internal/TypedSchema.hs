module Data.Bond.Internal.TypedSchema where

import Data.Bond.Types
import Data.Bond.Internal.OrdinalSet

import Data.Text
import Data.Typeable
import qualified Data.Map.Strict as M

data StructSchema = StructSchema
    { structTag :: TypeRep
    , structName :: Text
    , structQualifiedName :: Text
    , structAttrs :: M.Map Text Text
    , structBase :: Maybe StructSchema
    , structFields :: M.Map Ordinal FieldSchema
    , structOrdinalsRequiredOnWrite :: OrdinalSet
    , structOrdinalsRequiredOnRead :: OrdinalSet
    }

data FieldModifier = FieldOptional | FieldRequired | FieldRequiredOptional
data DefaultValue a = DefaultNothing | DefaultValue a

-- |Field type and default value
data FieldTypeInfo
    = FieldBool (DefaultValue Bool)
    | FieldInt8 (DefaultValue Int8)
    | FieldInt16 (DefaultValue Int16)
    | FieldInt32 (DefaultValue Int32)
    | FieldInt64 (DefaultValue Int64)
    | FieldUInt8 (DefaultValue Word8)
    | FieldUInt16 (DefaultValue Word16)
    | FieldUInt32 (DefaultValue Word32)
    | FieldUInt64 (DefaultValue Word64)
    | FieldFloat (DefaultValue Float)
    | FieldDouble (DefaultValue Double)
    | FieldString (DefaultValue Utf8)
    | FieldWString (DefaultValue Utf16)
    | FieldStruct (DefaultValue ()) StructSchema
    | FieldBonded (DefaultValue ()) StructSchema
    | FieldList (DefaultValue ()) ElementTypeInfo
    | FieldSet (DefaultValue ()) ElementTypeInfo
    | FieldMap (DefaultValue ()) ElementTypeInfo ElementTypeInfo

-- |Inner value type
data ElementTypeInfo
    = ElementBool
    | ElementInt8
    | ElementInt16
    | ElementInt32
    | ElementInt64
    | ElementUInt8
    | ElementUInt16
    | ElementUInt32
    | ElementUInt64
    | ElementFloat
    | ElementDouble
    | ElementString
    | ElementWString
    | ElementStruct StructSchema
    | ElementBonded StructSchema
    | ElementList ElementTypeInfo
    | ElementSet ElementTypeInfo
    | ElementMap ElementTypeInfo ElementTypeInfo

data FieldSchema = FieldSchema
    { fieldName :: Text
    , fieldAttrs :: M.Map Text Text
    , fieldModifier :: FieldModifier
    , fieldType :: FieldTypeInfo
    }
