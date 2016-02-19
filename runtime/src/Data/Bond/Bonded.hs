{-# LANGUAGE DeriveDataTypeable #-}
module Data.Bond.Bonded where

import Data.Bond.Marshal
import {-# SOURCE #-} Data.Bond.Proto
import {-# SOURCE #-} Data.Bond.Internal.Protocol

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as Lazy

-- | BondedException is thrown when attempt to deserialize bonded field for comparison fails.
-- | To handle such cases in the pure code explicitly decode all bonded fields before comparing structures.
data BondedException = BondedException String
    deriving (Show, Typeable)

instance Exception BondedException

data Bonded a = BondedStream Lazy.ByteString | BondedObject a
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

getValue :: BondStruct a => Bonded a -> Either String a
getValue (BondedObject a) = Right a
getValue (BondedStream s) = bondUnmarshal s

castValue :: BondStruct b => Bonded a -> Either String b
castValue (BondedObject _) = error "can't cast deserialized struct"
castValue (BondedStream s) = bondUnmarshal s

putValue :: a -> Bonded a
putValue = BondedObject

marshalValue :: (BondProto t, BondStruct a) => t -> a -> Either String (Bonded b)
marshalValue t = fmap BondedStream . bondMarshal' t
