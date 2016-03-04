{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Data.Bond.Types (
    Blob(..),
    Bonded(..),
    Bool,
    Double,
    EncodedString(..),
    Float,
    H.HashSet,
    S.Set,
    Int,
    Int16,
    Int32,
    Int64,
    Int8,
    Maybe,
    M.Map,
    Ordinal(..),
    Utf16(..),
    Utf8(..),
    V.Vector,
    Word16,
    Word32,
    Word64,
    Word8,
    fromString
  ) where

import {-# SOURCE #-} Data.Bond.Internal.Bonded

import Data.Data
import Data.Int
import Data.String
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- |Bond "string" type
newtype Utf8 = Utf8 BS.ByteString
    deriving (Eq, Ord, Hashable, Typeable)

-- |Bond "wstring" type
newtype Utf16 = Utf16 BS.ByteString
    deriving (Eq, Ord, Hashable, Typeable)

-- |Bond "blob" type
newtype Blob = Blob BS.ByteString
    deriving (Show, Eq, Ord, Hashable, Typeable)

-- |Bond string\/wstring transformations from/to 'String' and 'T.Text'.
class IsString a => EncodedString a where
    -- |Convert to 'String'
    toString :: a -> String
    toString = T.unpack . toText

    -- |Make bond string from 'T.Text'
    fromText :: T.Text -> a
    -- |Convert to 'T.Text'
    toText :: a -> T.Text

instance IsString Utf8 where
    fromString = fromText . T.pack

instance EncodedString Utf8 where
    fromText = Utf8 . T.encodeUtf8
    toText (Utf8 s) = T.decodeUtf8 s

instance IsString Utf16 where
    fromString = fromText . T.pack

instance EncodedString Utf16 where
    fromText = Utf16 . T.encodeUtf16LE
    toText (Utf16 s) = T.decodeUtf16LE s

instance Show Utf8 where show s = show $ toString s
instance Show Utf16 where show s = show $ toString s

-- |Bond structure field ordinal.
newtype Ordinal = Ordinal Word16
    deriving (Eq, Ord, Show, Hashable)
