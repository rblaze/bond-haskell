{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
module Data.Bond.Marshal (
    BondProto(bondMarshal, bondMarshalWithSchema),
    BondTaggedProto(bondMarshalTagged),
    bondUnmarshal,
    bondUnmarshalWithSchema,
    bondUnmarshalTagged,
    bondRecode,
    bondRecodeStruct,
    bondRecodeToTagged
  ) where

import Data.Bond.Schema.SchemaDef

import Data.Bond.Bonded
import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto
import Data.Bond.Schema
import Data.Bond.Struct

import Data.Proxy

import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondUnmarshal s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondRead FastBinaryProto rest
             | sig == protoSig CompactBinaryProto -> bondRead CompactBinaryProto rest
             | sig == protoSig CompactBinaryV1Proto -> bondRead CompactBinaryV1Proto rest
             | sig == protoSig SimpleBinaryProto -> bondRead SimpleBinaryProto rest
             | sig == protoSig SimpleBinaryV1Proto -> bondRead SimpleBinaryV1Proto rest
             | sig == protoSig JsonProto -> bondRead JsonProto rest
             | otherwise -> Left "unknown signature in marshalled stream"

bondUnmarshalWithSchema :: SchemaDef -> BL.ByteString -> Either String Struct
bondUnmarshalWithSchema schema s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondReadWithSchema FastBinaryProto schema rest
             | sig == protoSig CompactBinaryProto -> bondReadWithSchema CompactBinaryProto schema rest
             | sig == protoSig CompactBinaryV1Proto -> bondReadWithSchema CompactBinaryV1Proto schema rest
             | sig == protoSig SimpleBinaryProto -> bondReadWithSchema SimpleBinaryProto schema rest
             | sig == protoSig SimpleBinaryV1Proto -> bondReadWithSchema SimpleBinaryV1Proto schema rest
             | sig == protoSig JsonProto -> bondReadWithSchema JsonProto schema rest
             | otherwise -> Left "unknown signature in marshalled stream"

bondUnmarshalTagged :: BL.ByteString -> Either String Struct
bondUnmarshalTagged s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondReadTagged FastBinaryProto rest
             | sig == protoSig CompactBinaryProto -> bondReadTagged CompactBinaryProto rest
             | sig == protoSig CompactBinaryV1Proto -> bondReadTagged CompactBinaryV1Proto rest
             | sig == protoSig SimpleBinaryProto -> Left "SimpleBinaryProto does not support schemaless operations"
             | sig == protoSig SimpleBinaryV1Proto -> Left "SimpleBinaryV1Proto does not support schemaless operations"
             | sig == protoSig JsonProto -> Left "JsonProto does not support schemaless operations"
             | otherwise -> Left "unknown signature in marshalled stream"

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
    (sig, _) = BL.splitAt 4 stream
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
    (sig, _) = BL.splitAt 4 stream
    schema = getSchema (Proxy :: Proxy a)
    taggedSigs = [protoSig FastBinaryProto, protoSig CompactBinaryProto, protoSig CompactBinaryV1Proto]
    isTaggedSource = sig `elem` taggedSigs

bondRecodeStruct :: forall t. BondProto t => t -> SchemaDef -> Bonded Struct -> Either String (Bonded Struct)
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
    (sig, _) = BL.splitAt 4 stream
    taggedSigs = [protoSig FastBinaryProto, protoSig CompactBinaryProto, protoSig CompactBinaryV1Proto]
    isTaggedSource = sig `elem` taggedSigs
