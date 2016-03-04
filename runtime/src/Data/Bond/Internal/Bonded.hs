{-# LANGUAGE DeriveDataTypeable #-}
module Data.Bond.Internal.Bonded where

import Data.Bond.Marshal
import Data.Bond.Proto
import Data.Bond.Internal.MarshalUtils
import {-# SOURCE #-} Data.Bond.Internal.Protocol

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as Lazy

-- | BondedException is thrown when attempt to deserialize bonded field for comparison fails.
-- To handle such cases in the pure code explicitly decode all bonded fields before comparing structures.
data BondedException = BondedException String
    deriving (Show, Typeable)

instance Exception BondedException

-- | bonded\<T> value
data Bonded a
        = BondedStream Lazy.ByteString -- ^ Marshalled stream
        | BondedObject a               -- ^ Deserialized value
    deriving Typeable

instance Show a => Show (Bonded a) where
    show BondedStream{} = "BondedStream"
    show (BondedObject v) = show v

instance (BondStruct a, Eq a) => Eq (Bonded a) where
    a == b = let aobj = case getValue a of
                            Left msg -> throw (BondedException msg)
                            Right v -> v
                 bobj = case getValue b of
                            Left msg -> throw (BondedException msg)
                            Right v -> v
              in aobj == bobj

-- | Extract value from 'Bonded' using compile-type schema
getValue :: BondStruct a => Bonded a -> Either String a
getValue (BondedObject a) = Right a
getValue (BondedStream s) = bondUnmarshal s

-- | Extract value from 'Bonded' using compile-type schema for other type.
-- This may be useful for casting values to child structs.
-- User is responsible for schema compatibility.
castValue :: BondStruct b => Bonded a -> Either String b
castValue (BondedObject _) = error "can't cast deserialized struct"
castValue (BondedStream s) = bondUnmarshal s

-- | Put struct to the bonded\<T> field.
putValue :: a -> Bonded a
putValue = BondedObject

-- | Marshal struct to the bonded\<T> field.
-- There is no checks for schema compatibility, caveat emptor.
marshalValue :: (BondProto t, BondStruct a) => t -> a -> Either String (Bonded b)
marshalValue t = fmap BondedStream . bondMarshal' t
