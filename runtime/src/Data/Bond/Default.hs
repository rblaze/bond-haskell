module Data.Bond.Default (
    Default(..)
  ) where

import Data.Bond.Types
import {-# SOURCE #-} Data.Bond.Schema.Variant
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

class Default a where
    defaultValue :: a
    -- optimized type-aware comparison with default value
    equalToDefault :: Variant -> a -> Bool
    equalToDefault _ _ = False

instance Default Bool where
    defaultValue = False
    equalToDefault v a = (uint_value v /= 0) == a
instance Default Double where
    defaultValue = 0
    equalToDefault v a = double_value v == a
instance Default Float where
    defaultValue = 0
    equalToDefault v a = double_value v == realToFrac a
instance Default Int8 where
    defaultValue = 0
    equalToDefault v a = int_value v == fromIntegral a
instance Default Int16 where
    defaultValue = 0
    equalToDefault v a = int_value v == fromIntegral a
instance Default Int32 where
    defaultValue = 0
    equalToDefault v a = int_value v == fromIntegral a
instance Default Int64 where
    defaultValue = 0
    equalToDefault v a = int_value v == fromIntegral a
instance Default Word8 where
    defaultValue = 0
    equalToDefault v a = uint_value v == fromIntegral a
instance Default Word16 where
    defaultValue = 0
    equalToDefault v a = uint_value v == fromIntegral a
instance Default Word32 where
    defaultValue = 0
    equalToDefault v a = uint_value v == fromIntegral a
instance Default Word64 where
    defaultValue = 0
    equalToDefault v a = uint_value v == fromIntegral a
instance Default (Maybe a) where
    defaultValue = Nothing
    -- default value for nullable is always null
    equalToDefault _ = isNothing
instance Default [a] where
    defaultValue = []
    -- default value for list is always []
    equalToDefault _ = null
instance Default Blob where
    defaultValue = Blob BS.empty
    -- default value for blob is always BS.empty
    equalToDefault _ (Blob a) = BS.null a
instance Default Utf8 where
    defaultValue = Utf8 BS.empty
    -- FIXME can be optimized with preprocessing
    equalToDefault v a = string_value v == a
instance Default Utf16 where
    defaultValue = Utf16 BS.empty
    equalToDefault v a = wstring_value v == a
instance Default (Map a b) where
    defaultValue = M.empty
    -- default value for map is always M.empty
    equalToDefault _ = M.null
instance Default (HashSet a) where
    defaultValue = H.empty
    -- default value for set is always H.empty
    equalToDefault _ = H.null
instance Default (Set a) where
    defaultValue = S.empty
    -- default value for set is always H.empty
    equalToDefault _ = S.null
instance Default (Vector a) where
    defaultValue = V.empty
    -- default value for vector is always V.empty
    equalToDefault _ = V.null
instance Default a => Default (Bonded a) where
    defaultValue = BondedObject defaultValue
    -- Default value check is performed to decide if field needs to be written.
    -- Bonded streams must always be written.
    equalToDefault _ _ = False
