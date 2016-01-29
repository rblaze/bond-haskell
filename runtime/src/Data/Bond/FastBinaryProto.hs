{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies #-}
module Data.Bond.FastBinaryProto (
        FastBinaryProto(..)
    ) where

import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.TaggedProtocol
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Wire

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.ProtocolType

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Proxy
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data FastBinaryProto = FastBinaryProto

instance TaggedProtocol FastBinaryProto where
    getFieldHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- if t == bT_STOP || t == bT_STOP_BASE then return 0 else getWord16le
        return (t, Ordinal n)
    getListHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- getVarInt
        return (t, n)
    getTaggedStruct = getTaggedData
    putFieldHeader t (Ordinal n) = do
        putTag t
        putWord16le n
    putListHeader t n = do
        putTag t
        putVarInt n
    putTaggedStruct s = putTaggedData s >> putTag bT_STOP
    skipStruct =
        let loop = do
                (td, _) <- getFieldHeader
                if | td == bT_STOP -> return ()
                   | td == bT_STOP_BASE -> loop
                   | otherwise -> skipType td >> loop
         in loop
    skipRestOfStruct = skipType bT_STRUCT
    skipType t =
        if | t == bT_BOOL -> skip 1
           | t == bT_UINT8 -> skip 1
           | t == bT_UINT16 -> skip 2
           | t == bT_UINT32 -> skip 4
           | t == bT_UINT64 -> skip 8
           | t == bT_FLOAT -> skip 4
           | t == bT_DOUBLE -> skip 8
           | t == bT_STRING -> getVarInt >>= skip
           | t == bT_STRUCT ->
                let loop = do
                        td <- BondDataType . fromIntegral <$> getWord8
                        if | td == bT_STOP -> return ()
                           | td == bT_STOP_BASE -> loop
                           | otherwise -> skip 2 >> skipType td >> loop
                 in loop
           | t == bT_LIST -> do
                td <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
                replicateM_ n (skipType td)
           | t == bT_SET -> skipType bT_LIST
           | t == bT_MAP -> do
                tk <- BondDataType . fromIntegral <$> getWord8
                tv <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
                replicateM_ n $ skipType tk >> skipType tv
           | t == bT_INT8 -> skip 1
           | t == bT_INT16 -> skip 2
           | t == bT_INT32 -> skip 4
           | t == bT_INT64 -> skip 8
           | t == bT_WSTRING -> do
                n <- getVarInt
                skip $ n * 2
           | otherwise -> fail $ "Invalid type to skip " ++ show t

instance BondProto FastBinaryProto where
    bondRead = binaryDecode
    bondWrite = binaryEncode
    protoSig _ = protoHeader fAST_PROTOCOL 1

instance BondTaggedProto FastBinaryProto where
    bondReadTagged = readTagged
    bondWriteTagged = writeTagged

instance Protocol FastBinaryProto where
    type ReaderM FastBinaryProto = B.Get
    type WriterM FastBinaryProto = B.PutM

    bondGetStruct = getStruct TopLevelStruct
    bondGetBaseStruct = getStruct BaseStruct

    bondGetBool = do
        v <- getWord8
        return $ v /= 0
    bondGetUInt8 = getWord8
    bondGetUInt16 = getWord16le
    bondGetUInt32 = getWord32le
    bondGetUInt64 = getWord64le
    bondGetInt8 = fromIntegral <$> getWord8
    bondGetInt16 = fromIntegral <$> getWord16le
    bondGetInt32 = fromIntegral <$> getWord32le
    bondGetInt64 = fromIntegral <$> getWord64le
    bondGetFloat = wordToFloat <$> getWord32le
    bondGetDouble = wordToDouble <$> getWord64le
    bondGetString = do
        n <- getVarInt
        Utf8 <$> getByteString n
    bondGetWString = do
        n <- getVarInt
        Utf16 <$> getByteString (n * 2)
    bondGetBlob = do
        (t, n) <- getListHeader
        unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
        Blob <$> getByteString n
    bondGetDefNothing = Just <$> bondGet
    bondGetList = getList
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = getMap
    bondGetVector = getVector
    bondGetNullable = do
        v <- bondGetList
        case v of
            [] -> return Nothing
            [x] -> return (Just x)
            _ -> fail $ "list of length " ++ show (length v) ++ " where nullable expected"
    bondGetBonded = do
        size <- lookAhead $ do
            start <- bytesRead
            skipType bT_STRUCT
            stop <- bytesRead
            return (stop - start)
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream $ BL.append (protoHeader fAST_PROTOCOL 1) bs

    bondPutStruct = putStruct TopLevelStruct
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField p n (Just v) = putField p n v

    bondPutBool True = putWord8 1
    bondPutBool False = putWord8 0
    bondPutUInt8 = putWord8
    bondPutUInt16 = putWord16le
    bondPutUInt32 = putWord32le
    bondPutUInt64 = putWord64le
    bondPutInt8 = putWord8 . fromIntegral
    bondPutInt16 = putWord16le . fromIntegral
    bondPutInt32 = putWord32le . fromIntegral
    bondPutInt64 = putWord64le . fromIntegral
    bondPutFloat = putWord32le . floatToWord
    bondPutDouble = putWord64le . doubleToWord
    bondPutString (Utf8 s) = do
        putVarInt $ BS.length s
        putByteString s
    bondPutWString (Utf16 s) = do
        putVarInt $ BS.length s `div` 2
        putByteString s
    bondPutList = putList
    bondPutNullable = bondPutList . maybeToList
    bondPutHashSet = putHashSet
    bondPutSet = putSet
    bondPutMap = putMap
    bondPutVector = putVector
    bondPutBlob (Blob b) = do
        putTag bT_INT8
        putVarInt $ BS.length b
        putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream s) = putLazyByteString (BL.drop 4 s) -- FIXME handle different protocols

getList :: forall a. Serializable a => BondGet FastBinaryProto [a]
getList = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    replicateM n bondGet

getVector :: forall a. Serializable a => BondGet FastBinaryProto (Vector a)
getVector = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    V.replicateM n bondGet

getMap :: forall k v. (Ord k, Serializable k, Serializable v) => BondGet FastBinaryProto (Map k v)
getMap = do
    let etk = getWireType (Proxy :: Proxy k)
    let etv = getWireType (Proxy :: Proxy v)
    tk <- BondDataType . fromIntegral <$> getWord8
    tv <- BondDataType . fromIntegral <$> getWord8
    unless (tk == etk) $ fail $ "invalid element tag " ++ show tk ++ " in list field, " ++ show etk ++ " expected"
    unless (tv == etv) $ fail $ "invalid element tag " ++ show tv ++ " in list field, " ++ show etv ++ " expected"
    n <- getVarInt
    fmap M.fromList $ replicateM n $ do
        k <- bondGet
        v <- bondGet
        return (k, v)

putList :: forall a. Serializable a => [a] -> BondPut FastBinaryProto
putList xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    mapM_ bondPut xs

putHashSet :: forall a. Serializable a => HashSet a -> BondPut FastBinaryProto
putHashSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    mapM_ bondPut $ H.toList xs

putSet :: forall a. Serializable a => Set a -> BondPut FastBinaryProto
putSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    mapM_ bondPut $ S.toList xs

putMap :: forall k v. (Serializable k, Serializable v) => Map k v -> BondPut FastBinaryProto
putMap m = do
    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        bondPut k
        bondPut v

putVector :: forall a. Serializable a => Vector a -> BondPut FastBinaryProto
putVector xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    V.mapM_ bondPut xs
