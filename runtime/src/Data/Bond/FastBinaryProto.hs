{-# Language ScopedTypeVariables, EmptyDataDecls, MultiWayIf, TypeFamilies #-}
module Data.Bond.FastBinaryProto (
        FastBinaryProto
    ) where

import Data.Bond.Cast
import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.TaggedProtocol
import Data.Bond.Types
import Data.Bond.Wire
import qualified Data.Bond.Utils as U

import Data.Bond.Schema.BondDataType

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.List
import Data.Maybe
import Data.Proxy
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data FastBinaryProto

instance TaggedProtocol FastBinaryProto where
    getFieldHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- if t == bT_STOP || t == bT_STOP_BASE then return 0 else getWord16le
        return (t, Ordinal n)
    putFieldHeader t (Ordinal n) = do
        putTag t
        putWord16le n
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
    type ReaderM FastBinaryProto = ReaderT GetContext B.Get
    type WriterM FastBinaryProto = ReaderT PutContext B.PutM

    bondDecode = binaryDecode
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
        t <- BondDataType . fromIntegral <$> getWord8
        unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
        n <- getVarInt
        Blob <$> getByteString n
    bondGetDefNothing = Just <$> bondGet
    bondGetList = do
        t <- BondDataType . fromIntegral <$> getWord8
        elemtype <- checkElementGetType t
        n <- getVarInt
        getAs elemtype $ replicateM n bondGet
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = do
        tk <- BondDataType . fromIntegral <$> getWord8
        tv <- BondDataType . fromIntegral <$> getWord8
        keytype <- checkKeyGetType tk
        elemtype <- checkElementGetType tv
        n <- getVarInt
        fmap M.fromList $ replicateM n $ do
            k <- getAs keytype bondGet
            v <- getAs elemtype bondGet
            return (k, v)
    bondGetVector = do
        t <- BondDataType . fromIntegral <$> getWord8
        elemtype <- checkElementGetType t
        n <- getVarInt
        getAs elemtype $ V.replicateM n bondGet
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
        return $ BondedStream bs

    bondEncode = binaryEncode
    bondPutStruct = putStruct TopLevelStruct
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField n (Just v) = putField n v

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
    bondPutBonded (BondedStream s) = putLazyByteString s -- FIXME handle different protocols

putList :: forall a. BondSerializable a => [a] -> BondPut FastBinaryProto
putList xs = do
    t <- checkPutContainerType bT_LIST

    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ length xs
    putAs t $ mapM_ bondPut xs

putHashSet :: forall a. BondSerializable a => HashSet a -> BondPut FastBinaryProto
putHashSet xs = do
    t <- checkPutContainerType bT_SET

    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ H.size xs
    putAs t $ mapM_ bondPut $ H.toList xs

putSet :: forall a. BondSerializable a => Set a -> BondPut FastBinaryProto
putSet xs = do
    t <- checkPutContainerType bT_SET

    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ S.size xs
    putAs t $ mapM_ bondPut $ S.toList xs

putMap :: forall k v. (BondSerializable k, BondSerializable v) => Map k v -> BondPut FastBinaryProto
putMap m = do
    (tk, tv) <- checkPutMapType

    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        putAs tk $ bondPut k
        putAs tv $ bondPut v

putVector :: forall a. BondSerializable a => Vector a -> BondPut FastBinaryProto
putVector xs = do
    t <- checkPutContainerType bT_LIST

    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ V.length xs
    putAs t $ V.mapM_ bondPut xs

getVarInt :: (FiniteBits a, Num a) => BondGet FastBinaryProto a
getVarInt = BondGet $ lift $ U.getVarInt

putVarInt :: (FiniteBits a, Integral a) => a -> BondPut FastBinaryProto
putVarInt = BondPut . lift . U.putVarInt
