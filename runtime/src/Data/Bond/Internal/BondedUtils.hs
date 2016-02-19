{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bond.Internal.BondedUtils where

import {-# SOURCE #-} Data.Bond.CompactBinaryProto
import {-# SOURCE #-} Data.Bond.FastBinaryProto
import Data.Bond.Bonded
import Data.Bond.Marshal
import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.TypedSchema

import Control.Applicative
import Data.Proxy
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.ByteString.Lazy as BL

bondRecode :: forall t a. (BondProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
bondRecode t (BondedObject a) = BondedStream <$> bondMarshal t a
bondRecode t (BondedStream stream)
    | sig == protoSig t = Right $ BondedStream stream
    | isTaggedSource = do
        v <- bondUnmarshalTagged stream
        s <- bondMarshalWithSchema t schema v
        return (BondedStream s)
    | otherwise = do
        v <- bondUnmarshalWithSchema schema stream
        s <- bondMarshalWithSchema t schema v
        return (BondedStream s)
    where
    sig = BL.take 4 stream
    schema = getSchema (Proxy :: Proxy a)
    taggedSigs = [protoSig FastBinaryProto, protoSig CompactBinaryProto, protoSig CompactBinaryV1Proto]
    isTaggedSource = sig `elem` taggedSigs

bondRecodeToTagged :: forall t a. (BondTaggedProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
bondRecodeToTagged t (BondedObject a) = BondedStream <$> bondMarshal t a
bondRecodeToTagged t (BondedStream stream)
    | sig == protoSig t = Right $ BondedStream stream
    | isTaggedSource = do
        v <- bondUnmarshalTagged stream
        s <- bondMarshalTagged t v
        return (BondedStream s)
    | otherwise = do
        v <- bondUnmarshalWithSchema schema stream
        s <- bondMarshalWithSchema t schema v
        return (BondedStream s)
    where
    sig = BL.take 4 stream
    schema = getSchema (Proxy :: Proxy a)
    taggedSigs = [protoSig FastBinaryProto, protoSig CompactBinaryProto, protoSig CompactBinaryV1Proto]
    isTaggedSource = sig `elem` taggedSigs

bondRecodeStruct :: BondProto t => t -> StructSchema -> Bonded Struct -> Either String (Bonded Struct)
bondRecodeStruct t schema (BondedObject a) = BondedStream <$> bondMarshalWithSchema t schema a
bondRecodeStruct t schema (BondedStream stream)
    | sig == protoSig t = Right $ BondedStream stream
    | isTaggedSource = do
        v <- bondUnmarshalTagged stream
        s <- bondMarshalWithSchema t schema v
        return (BondedStream s)
    | otherwise = do
        v <- bondUnmarshalWithSchema schema stream
        s <- bondMarshalWithSchema t schema v
        return (BondedStream s)
    where
    sig = BL.take 4 stream
    taggedSigs = [protoSig FastBinaryProto, protoSig CompactBinaryProto, protoSig CompactBinaryV1Proto]
    isTaggedSource = sig `elem` taggedSigs
