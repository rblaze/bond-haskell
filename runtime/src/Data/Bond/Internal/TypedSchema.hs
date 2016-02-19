{-# LANGUAGE OverloadedStrings #-}
module Data.Bond.Internal.TypedSchema where

import Data.Bond.Types
import Data.Bond.Internal.OrdinalSet

import Data.Text
import Data.Typeable
import qualified Data.Map.Strict as M

data DefaultValue a = DefaultNothing | DefaultValue a

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

data FieldModifier = FieldOptional | FieldRequired | FieldRequiredOptional
    deriving Eq

data FieldSchema = FieldSchema
    { fieldName :: Text
    , fieldAttrs :: M.Map Text Text
    , fieldModifier :: FieldModifier
    , fieldType :: FieldTypeInfo
    }

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

fieldToElementType :: FieldTypeInfo -> ElementTypeInfo
fieldToElementType (FieldBool _) = ElementBool
fieldToElementType (FieldInt8 _) = ElementInt8
fieldToElementType (FieldInt16 _) = ElementInt16
fieldToElementType (FieldInt32 _) = ElementInt32
fieldToElementType (FieldInt64 _) = ElementInt64
fieldToElementType (FieldUInt8 _) = ElementUInt8
fieldToElementType (FieldUInt16 _) = ElementUInt16
fieldToElementType (FieldUInt32 _) = ElementUInt32
fieldToElementType (FieldUInt64 _) = ElementUInt64
fieldToElementType (FieldFloat _) = ElementFloat
fieldToElementType (FieldDouble _) = ElementDouble
fieldToElementType (FieldString _) = ElementString
fieldToElementType (FieldWString _) = ElementWString
fieldToElementType (FieldStruct _ schema) = ElementStruct schema
fieldToElementType (FieldBonded _ schema) = ElementBonded schema
fieldToElementType (FieldList _ element) = ElementList element
fieldToElementType (FieldSet _ element) = ElementSet element
fieldToElementType (FieldMap _ key value) = ElementMap key value

elementToFieldType :: ElementTypeInfo -> FieldTypeInfo
elementToFieldType ElementBool = FieldBool (DefaultValue False)
elementToFieldType ElementInt8 = FieldInt8 (DefaultValue 0)
elementToFieldType ElementInt16 = FieldInt16 (DefaultValue 0)
elementToFieldType ElementInt32 = FieldInt32 (DefaultValue 0)
elementToFieldType ElementInt64 = FieldInt64 (DefaultValue 0)
elementToFieldType ElementUInt8 = FieldUInt8 (DefaultValue 0)
elementToFieldType ElementUInt16 = FieldUInt16 (DefaultValue 0)
elementToFieldType ElementUInt32 = FieldUInt32 (DefaultValue 0)
elementToFieldType ElementUInt64 = FieldUInt64 (DefaultValue 0)
elementToFieldType ElementFloat = FieldFloat (DefaultValue 0)
elementToFieldType ElementDouble = FieldDouble (DefaultValue 0)
elementToFieldType ElementString = FieldString (DefaultValue "")
elementToFieldType ElementWString = FieldWString (DefaultValue "")
elementToFieldType (ElementStruct schema) = FieldStruct (DefaultValue ()) schema
elementToFieldType (ElementBonded schema) = FieldBonded (DefaultValue ()) schema
elementToFieldType (ElementList element) = FieldList (DefaultValue ()) element
elementToFieldType (ElementSet element) = FieldSet (DefaultValue ()) element
elementToFieldType (ElementMap key value) = FieldMap (DefaultValue ()) key value

elementToDefNothingFieldType :: ElementTypeInfo -> FieldTypeInfo
elementToDefNothingFieldType ElementBool = FieldBool DefaultNothing
elementToDefNothingFieldType ElementInt8 = FieldInt8 DefaultNothing
elementToDefNothingFieldType ElementInt16 = FieldInt16 DefaultNothing
elementToDefNothingFieldType ElementInt32 = FieldInt32 DefaultNothing
elementToDefNothingFieldType ElementInt64 = FieldInt64 DefaultNothing
elementToDefNothingFieldType ElementUInt8 = FieldUInt8 DefaultNothing
elementToDefNothingFieldType ElementUInt16 = FieldUInt16 DefaultNothing
elementToDefNothingFieldType ElementUInt32 = FieldUInt32 DefaultNothing
elementToDefNothingFieldType ElementUInt64 = FieldUInt64 DefaultNothing
elementToDefNothingFieldType ElementFloat = FieldFloat DefaultNothing
elementToDefNothingFieldType ElementDouble = FieldDouble DefaultNothing
elementToDefNothingFieldType ElementString = FieldString DefaultNothing
elementToDefNothingFieldType ElementWString = FieldWString DefaultNothing
elementToDefNothingFieldType (ElementStruct schema) = FieldStruct DefaultNothing schema
elementToDefNothingFieldType (ElementBonded schema) = FieldBonded DefaultNothing schema
elementToDefNothingFieldType (ElementList element) = FieldList DefaultNothing element
elementToDefNothingFieldType (ElementSet element) = FieldSet DefaultNothing element
elementToDefNothingFieldType (ElementMap key value) = FieldMap DefaultNothing key value
