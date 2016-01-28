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
    Word8
  ) where

import Data.Bond.Bonded

import Data.Data
import Data.Int
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

newtype Utf8 = Utf8 BS.ByteString
    deriving (Eq, Ord, Hashable, Typeable)

newtype Utf16 = Utf16 BS.ByteString
    deriving (Eq, Ord, Hashable, Typeable)

newtype Blob = Blob BS.ByteString
    deriving (Show, Eq, Ord, Hashable, Typeable)

class EncodedString a where
    fromString :: String -> a
    fromText :: T.Text -> a

instance EncodedString Utf8 where
    fromString = fromText . T.pack
    fromText = Utf8 . T.encodeUtf8

instance EncodedString Utf16 where
    fromString = fromText . T.pack
    fromText = Utf16 . T.encodeUtf16LE

instance Show Utf8 where show (Utf8 s) = show $ T.unpack $ T.decodeUtf8 s
instance Show Utf16 where show (Utf16 s) = show $ T.unpack $ T.decodeUtf16LE s

newtype Ordinal = Ordinal Word16
    deriving (Eq, Ord, Show)
