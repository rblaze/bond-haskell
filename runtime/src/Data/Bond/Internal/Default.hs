module Data.Bond.Internal.Default where

import Data.Bond.TypedSchema
import Data.Bond.Types
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- |Type with default value.
--  Used for optional field elimination.
class Default a where
    -- |Get default value for specified type.
    defaultValue :: a
    -- |Check if value matches default in 'FieldTypeInfo'
    equalToDefault :: FieldTypeInfo -> a -> Bool
    equalToDefault _ _ = False

instance Default Bool where
    defaultValue = False
    equalToDefault (FieldBool (DefaultValue v)) a = v == a
    equalToDefault (FieldBool DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Bool"
instance Default Double where
    defaultValue = 0
    equalToDefault (FieldDouble (DefaultValue v)) a = v == a
    equalToDefault (FieldDouble DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Double"
instance Default Float where
    defaultValue = 0
    equalToDefault (FieldFloat (DefaultValue v)) a = v == a
    equalToDefault (FieldFloat DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Float"
instance Default Int8 where
    defaultValue = 0
    equalToDefault (FieldInt8 (DefaultValue v)) a = v == a
    equalToDefault (FieldInt8 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Int8"
instance Default Int16 where
    defaultValue = 0
    equalToDefault (FieldInt16 (DefaultValue v)) a = v == a
    equalToDefault (FieldInt16 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Int16"
instance Default Int32 where
    defaultValue = 0
    equalToDefault (FieldInt32 (DefaultValue v)) a = v == a
    equalToDefault (FieldInt32 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Int32"
instance Default Int64 where
    defaultValue = 0
    equalToDefault (FieldInt64 (DefaultValue v)) a = v == a
    equalToDefault (FieldInt64 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Int64"
instance Default Word8 where
    defaultValue = 0
    equalToDefault (FieldUInt8 (DefaultValue v)) a = v == a
    equalToDefault (FieldUInt8 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in UInt8"
instance Default Word16 where
    defaultValue = 0
    equalToDefault (FieldUInt16 (DefaultValue v)) a = v == a
    equalToDefault (FieldUInt16 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in UInt16"
instance Default Word32 where
    defaultValue = 0
    equalToDefault (FieldUInt32 (DefaultValue v)) a = v == a
    equalToDefault (FieldUInt32 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in UInt32"
instance Default Word64 where
    defaultValue = 0
    equalToDefault (FieldUInt64 (DefaultValue v)) a = v == a
    equalToDefault (FieldUInt64 DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in UInt64"
instance Default (Maybe a) where
    defaultValue = Nothing
    -- default value for nullable is always null
    equalToDefault FieldList{} = isNothing
    equalToDefault _ = error "internal error: field type do not match value in Maybe"
instance Default [a] where
    defaultValue = []
    -- default value for list is always []
    equalToDefault FieldList{} = null
    equalToDefault _ = error "internal error: field type do not match value in List"
instance Default Blob where
    defaultValue = Blob BS.empty
    -- default value for blob is always BS.empty
    equalToDefault FieldList{} (Blob a) = BS.null a
    equalToDefault _ _ = error "internal error: field type do not match value in Blob"
instance Default Utf8 where
    defaultValue = Utf8 BS.empty
    equalToDefault (FieldString (DefaultValue v)) a = v == a
    equalToDefault (FieldString DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in String"
instance Default Utf16 where
    defaultValue = Utf16 BS.empty
    equalToDefault (FieldWString (DefaultValue v)) a = v == a
    equalToDefault (FieldWString DefaultNothing) _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in WString"
instance Default (Map a b) where
    defaultValue = M.empty
    -- default value for map is always M.empty
    equalToDefault FieldMap{} = M.null
    equalToDefault _ = error "internal error: field type do not match value in Map"
instance Default (HashSet a) where
    defaultValue = H.empty
    -- default value for set is always H.empty
    equalToDefault FieldSet{} = H.null
    equalToDefault _ = error "internal error: field type do not match value in HashSet"
instance Default (Set a) where
    defaultValue = S.empty
    -- default value for set is always H.empty
    equalToDefault FieldSet{} = S.null
    equalToDefault _ = error "internal error: field type do not match value in Set"
instance Default (Vector a) where
    defaultValue = V.empty
    -- default value for vector is always V.empty
    equalToDefault FieldList{} = V.null
    equalToDefault _ = error "internal error: field type do not match value in Vector"
instance Default a => Default (Bonded a) where
    defaultValue = BondedObject defaultValue
    -- Default value check is performed to decide if field needs to be written.
    -- Bonded streams must always be written.
    equalToDefault FieldBonded{} _ = False
    equalToDefault _ _ = error "internal error: field type do not match value in Bonded"
