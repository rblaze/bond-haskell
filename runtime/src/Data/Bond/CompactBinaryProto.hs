{-# Language ScopedTypeVariables, EmptyDataDecls, MultiWayIf, TypeFamilies, ConstraintKinds #-}
module Data.Bond.CompactBinaryProto (
        CompactBinaryProto,
        CompactBinaryV1Proto
    ) where

import Data.Bond.Cast
import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.TaggedProtocol
import Data.Bond.Types
import Data.Bond.Wire
import Data.Bond.ZigZag
import qualified Data.Bond.Utils as U

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.Reader
import Data.Binary.Put (runPut)
import Data.Bits
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

import Data.Bond.Schema.BondDataType

data CompactBinaryProto
data CompactBinaryV1Proto

class TaggedProtocol t => CompactProtocol t where
    getListHeader :: ReaderM t ~ ReaderT c B.Get => BondGet t (BondDataType, Int)
    putListHeader :: (Integral a, FiniteBits a, WriterM t ~ ReaderT c B.PutM) => BondDataType -> a -> BondPut t

instance CompactProtocol CompactBinaryProto where
    getListHeader = do
        v <- getWord8
        if v `shiftR` 5 /= 0
            then return (BondDataType $ fromIntegral (v .&. 0x1f), fromIntegral (v `shiftR` 5) - 1)
            else do
                n <- getVarInt
                return (BondDataType (fromIntegral v), n)
    putListHeader t n = do
        let tag = fromIntegral $ fromEnum t
        if n < 7
            then putWord8 $ tag .|. fromIntegral ((1 + n) `shiftL` 5)
            else do
                putWord8 tag
                putVarInt n

instance TaggedProtocol CompactBinaryProto where
    getFieldHeader = getCompactFieldHeader
    putFieldHeader = putCompactFieldHeader
    skipStruct = getVarInt >>= skip
    skipRestOfStruct =
        let loop = do
                (wiretype, _) <- getFieldHeader
                if | wiretype == bT_STOP -> return ()
                   | wiretype == bT_STOP_BASE -> loop
                   | otherwise -> skipType wiretype >> loop
         in loop
    skipType = compactSkipType

instance BondProto CompactBinaryProto where
    type ReaderM CompactBinaryProto = ReaderT GetContext B.Get
    type WriterM CompactBinaryProto = ReaderT PutContext B.PutM

    bondDecode = binaryDecode
    bondGetStruct = do
        size <- getVarInt
        isolate size $ getStruct TopLevelStruct
    bondGetBaseStruct = getStruct BaseStruct

    bondGetBool = do
        v <- getWord8
        return $ v /= 0
    bondGetUInt8 = getWord8
    bondGetUInt16 = getVarInt
    bondGetUInt32 = getVarInt
    bondGetUInt64 = getVarInt
    bondGetInt8 = fromIntegral <$> getWord8
    bondGetInt16 = fromZigZag <$> getVarInt
    bondGetInt32 = fromZigZag <$> getVarInt
    bondGetInt64 = fromZigZag <$> getVarInt
    bondGetFloat = wordToFloat <$> getWord32le
    bondGetDouble = wordToDouble <$> getWord64le
    bondGetString = do
        n <- getVarInt
        Utf8 <$> getByteString n
    bondGetWString = do
        n <- getVarInt
        Utf16 <$> getByteString (n * 2)
    bondGetBlob = getBlob
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
    bondGetBonded = getBonded

    bondEncode = binaryEncode
    bondPutStruct v = do
        env <- BondPut ask
        let BondPut g = putStruct TopLevelStruct v :: BondPut CompactBinaryProto
        let bs = runPut (runReaderT g env)
        putVarInt $ BL.length bs
        putLazyByteString bs
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField n (Just v) = putField n v

    bondPutBool True = putWord8 1
    bondPutBool False = putWord8 0
    bondPutUInt8 = putWord8
    bondPutUInt16 = putVarInt
    bondPutUInt32 = putVarInt
    bondPutUInt64 = putVarInt
    bondPutInt8 = putWord8 . fromIntegral
    bondPutInt16 = putVarInt . toZigZag
    bondPutInt32 = putVarInt . toZigZag
    bondPutInt64 = putVarInt . toZigZag
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
        putListHeader bT_INT8 (BS.length b)
        putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream s) = putLazyByteString s -- FIXME handle different protocols

instance CompactProtocol CompactBinaryV1Proto where
    getListHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- getVarInt
        return (t, n)
    putListHeader t n = do
        putTag t
        putVarInt n

instance TaggedProtocol CompactBinaryV1Proto where
    getFieldHeader = getCompactFieldHeader
    putFieldHeader = putCompactFieldHeader
    skipStruct =
        let loop = do
                (td, _) <- getFieldHeader
                if | td == bT_STOP -> return ()
                   | td == bT_STOP_BASE -> loop
                   | otherwise -> skipType td >> loop
         in loop
    skipRestOfStruct = skipType bT_STRUCT
    skipType = compactSkipType

instance BondProto CompactBinaryV1Proto where
    type ReaderM CompactBinaryV1Proto = ReaderT GetContext B.Get
    type WriterM CompactBinaryV1Proto = ReaderT PutContext B.PutM

    bondDecode = binaryDecode
    bondGetStruct = getStruct TopLevelStruct
    bondGetBaseStruct = getStruct BaseStruct

    bondGetBool = do
        v <- getWord8
        return $ v /= 0
    bondGetUInt8 = getWord8
    bondGetUInt16 = getVarInt
    bondGetUInt32 = getVarInt
    bondGetUInt64 = getVarInt
    bondGetInt8 = fromIntegral <$> getWord8
    bondGetInt16 = fromZigZag <$> getVarInt
    bondGetInt32 = fromZigZag <$> getVarInt
    bondGetInt64 = fromZigZag <$> getVarInt
    bondGetFloat = wordToFloat <$> getWord32le
    bondGetDouble = wordToDouble <$> getWord64le
    bondGetString = do
        n <- getVarInt
        Utf8 <$> getByteString n
    bondGetWString = do
        n <- getVarInt
        Utf16 <$> getByteString (n * 2)
    bondGetBlob = getBlob
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
    bondGetBonded = getBonded

    bondEncode = binaryEncode
    bondPutStruct = putStruct TopLevelStruct
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField n (Just v) = putField n v

    bondPutBool True = putWord8 1
    bondPutBool False = putWord8 0
    bondPutUInt8 = putWord8
    bondPutUInt16 = putVarInt
    bondPutUInt32 = putVarInt
    bondPutUInt64 = putVarInt
    bondPutInt8 = putWord8 . fromIntegral
    bondPutInt16 = putVarInt . toZigZag
    bondPutInt32 = putVarInt . toZigZag
    bondPutInt64 = putVarInt . toZigZag
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
        putListHeader bT_INT8 (BS.length b)
        putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream s) = putLazyByteString s -- FIXME handle different protocols

getCompactFieldHeader :: (BondProto t, ReaderM t ~ ReaderT c B.Get) => BondGet t (BondDataType, Ordinal)
getCompactFieldHeader = do
    tag <- getWord8
    case tag `shiftR` 5 of
        6 -> do
            n <- getWord8
            return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal (fromIntegral n))
        7 -> do
            n <- getWord16le
            return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal n)
        n -> return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal (fromIntegral n))

putCompactFieldHeader :: (BondProto t, WriterM t ~ ReaderT c B.PutM) => BondDataType -> Ordinal -> BondPut t
putCompactFieldHeader t (Ordinal n) =
    let tbits = fromIntegral $ fromEnum t
        nbits = fromIntegral n
     in if | n <= 5 -> putWord8 $ tbits .|. (nbits `shiftL` 5)
           | n <= 255 -> do
                    putWord8 $ tbits .|. 0xC0
                    putWord8 nbits
           | otherwise -> do
                    putWord8 $ tbits .|. 0xE0
                    putWord16le n

getBlob :: (CompactProtocol t, ReaderM t ~ ReaderT c B.Get) => BondGet t Blob
getBlob = do
    (t, n) <- getListHeader
    unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
    Blob <$> getByteString n

getList :: (CompactProtocol t, ReaderM t ~ ReaderT GetContext B.Get, BondSerializable a) => BondGet t [a]
getList = do
    (t, n) <- getListHeader
    elemtype <- checkElementGetType t
    getAs elemtype $ replicateM n bondGet

getVector :: (CompactProtocol t, ReaderM t ~ ReaderT GetContext B.Get, BondSerializable a) => BondGet t (Vector a)
getVector = do
    (t, n) <- getListHeader
    elemtype <- checkElementGetType t
    getAs elemtype $ V.replicateM n bondGet

getMap :: (TaggedProtocol t, ReaderM t ~ ReaderT GetContext B.Get, Ord k, BondSerializable k, BondSerializable v) => BondGet t (Map k v)
getMap = do
    tk <- BondDataType . fromIntegral <$> getWord8
    tv <- BondDataType . fromIntegral <$> getWord8
    keytype <- checkKeyGetType tk
    elemtype <- checkElementGetType tv
    n <- getVarInt
    fmap M.fromList $ replicateM n $ do
        k <- getAs keytype bondGet
        v <- getAs elemtype bondGet
        return (k, v)

getBonded :: (TaggedProtocol t, ReaderM t ~ ReaderT c B.Get)  => BondGet t (Bonded a)
getBonded = do
    size <- lookAhead $ do
        start <- bytesRead
        skipType bT_STRUCT
        stop <- bytesRead
        return (stop - start)
    bs <- getLazyByteString (fromIntegral size)
    return $ BondedStream bs

skipVarInt :: forall t c. (BondProto t, ReaderM t ~ ReaderT c B.Get) => BondGet t ()
skipVarInt = void (getVarInt :: BondGet t Word64)

compactSkipType :: (CompactProtocol t, ReaderM t ~ ReaderT GetContext B.Get) => BondDataType -> BondGet t ()
compactSkipType t =
     if | t == bT_BOOL -> skip 1
        | t == bT_UINT8 -> skip 1
        | t == bT_UINT16 -> skipVarInt
        | t == bT_UINT32 -> skipVarInt
        | t == bT_UINT64 -> skipVarInt
        | t == bT_FLOAT -> skip 4
        | t == bT_DOUBLE -> skip 8
        | t == bT_STRING -> getVarInt >>= skip
        | t == bT_STRUCT -> skipStruct
        | t == bT_LIST -> do
            (td, n) <- getListHeader
            replicateM_ n (skipType td)
        | t == bT_SET -> skipType bT_LIST
        | t == bT_MAP -> do
            tk <- BondDataType . fromIntegral <$> getWord8
            tv <- BondDataType . fromIntegral <$> getWord8
            n <- getVarInt
            replicateM_ n $ skipType tk >> skipType tv
        | t == bT_INT8 -> skip 1
        | t == bT_INT16 -> skipVarInt
        | t == bT_INT32 -> skipVarInt
        | t == bT_INT64 -> skipVarInt
        | t == bT_WSTRING -> do
            n <- getVarInt
            skip $ n * 2
        | otherwise -> fail $ "Invalid type to skip " ++ show t

putList :: forall a t. (CompactProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable a) => [a] -> BondPut t
putList xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    putAs t $ mapM_ bondPut xs

putHashSet :: forall a t. (CompactProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable a) => HashSet a -> BondPut t
putHashSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    putAs t $ mapM_ bondPut $ H.toList xs

putSet :: forall a t. (CompactProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable a) => Set a -> BondPut t
putSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    putAs t $ mapM_ bondPut $ S.toList xs

putMap :: forall k v t. (BondProto t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable k, BondSerializable v) => Map k v -> BondPut t
putMap m = do
    (tk, tv) <- checkPutMapType

    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        putAs tk $ bondPut k
        putAs tv $ bondPut v

putVector :: forall a t. (CompactProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable a) => Vector a -> BondPut t
putVector xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    putAs t $ V.mapM_ bondPut xs

getVarInt :: (FiniteBits a, Num a, ReaderM t ~ ReaderT c B.Get) => BondGet t a
getVarInt = BondGet $ lift $ U.getVarInt

putVarInt :: (FiniteBits a, Integral a, WriterM t ~ ReaderT c B.PutM) => a -> BondPut t
putVarInt = BondPut . lift . U.putVarInt
