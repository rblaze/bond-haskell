{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Data.Bond.CompactBinaryProto (
        CompactBinaryProto(..),
        CompactBinaryV1Proto(..)
    ) where

import Data.Bond.BinaryClass
import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.TaggedProtocol
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Wire
import Data.Bond.ZigZag
import Data.Bond.Internal.BondedUtils
import Data.Bond.Internal.Protocol

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.ProtocolType

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.Error
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

data CompactBinaryProto = CompactBinaryProto
data CompactBinaryV1Proto = CompactBinaryV1Proto

instance TaggedProtocol CompactBinaryProto where
    getFieldHeader = getCompactFieldHeader
    getListHeader = do
        v <- getWord8
        if v `shiftR` 5 /= 0
            then return (BondDataType $ fromIntegral (v .&. 0x1f), fromIntegral (v `shiftR` 5) - 1)
            else do
                n <- getVarInt
                return (BondDataType (fromIntegral v), n)
    getTaggedStruct = do
        size <- getVarInt
        isolate size getTaggedData
    putFieldHeader = putCompactFieldHeader
    putListHeader t n = do
        let tag = fromIntegral $ fromEnum t
        if n < 7
            then putWord8 $ tag .|. fromIntegral ((1 + n) `shiftL` 5)
            else do
                putWord8 tag
                putVarInt n
    putTaggedStruct v = do
        let BondPut g = putTaggedData v >> putTag bT_STOP :: BondPut CompactBinaryProto
        case tryPut g of
            Left msg -> throwError msg
            Right bs -> do
                putVarInt $ BL.length bs
                putLazyByteString bs
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
    bondRead = binaryDecode
    bondWrite = binaryEncode
    bondReadWithSchema = readTaggedWithSchema
    bondWriteWithSchema = writeTaggedWithSchema
    protoSig _ = protoHeader cOMPACT_PROTOCOL 2

instance BondTaggedProto CompactBinaryProto where
    bondReadTagged = readTagged
    bondWriteTagged = writeTagged

instance Protocol CompactBinaryProto where
    type ReaderM CompactBinaryProto = B.Get
    type WriterM CompactBinaryProto = ErrorT String B.PutM

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
    bondGetBonded = getBonded cOMPACT_PROTOCOL 2

    bondPutStruct v = do
        let BondPut g = putStruct TopLevelStruct v :: BondPut CompactBinaryProto
        case tryPut g of
            Left msg -> throwError msg
            Right bs -> do
                putVarInt $ BL.length bs
                putLazyByteString bs
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField p n (Just v) = putField p n v

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
    bondPutBonded s = do
        BondedStream stream <- case bondRecode CompactBinaryProto s of
            Left msg -> throwError $ "Bonded recode error: " ++ msg
            Right v -> return v
        putLazyByteString (BL.drop 4 stream)

instance TaggedProtocol CompactBinaryV1Proto where
    getFieldHeader = getCompactFieldHeader
    getListHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- getVarInt
        return (t, n)
    getTaggedStruct = getTaggedData
    putFieldHeader = putCompactFieldHeader
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
    skipType = compactSkipType

instance BondProto CompactBinaryV1Proto where
    bondRead = binaryDecode
    bondWrite = binaryEncode
    bondReadWithSchema = readTaggedWithSchema
    bondWriteWithSchema = writeTaggedWithSchema
    protoSig _ = protoHeader cOMPACT_PROTOCOL 1

instance BondTaggedProto CompactBinaryV1Proto where
    bondReadTagged = readTagged
    bondWriteTagged = writeTagged

instance Protocol CompactBinaryV1Proto where
    type ReaderM CompactBinaryV1Proto = B.Get
    type WriterM CompactBinaryV1Proto = ErrorT String B.PutM

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
    bondGetBonded = getBonded cOMPACT_PROTOCOL 1

    bondPutStruct = putStruct TopLevelStruct
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ _ Nothing = return () -- FIXME check for required
    bondPutDefNothingField p n (Just v) = putField p n v

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
    bondPutBonded s = do
        BondedStream stream <- case bondRecode CompactBinaryV1Proto s of
            Left msg -> throwError $ "Bonded recode error: " ++ msg
            Right v -> return v
        putLazyByteString (BL.drop 4 stream)

getCompactFieldHeader :: (BondProto t, ReaderM t ~ B.Get) => BondGet t (BondDataType, Ordinal)
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

putCompactFieldHeader :: (BondProto t, BinaryPut (BondPutM t)) => BondDataType -> Ordinal -> BondPut t
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

getBlob :: (TaggedProtocol t, ReaderM t ~ B.Get) => BondGet t Blob
getBlob = do
    (t, n) <- getListHeader
    unless (t == bT_INT8) $ fail $ "invalid element tag " ++ bondTypeName t ++ " in blob field"
    Blob <$> getByteString n

getList :: forall a t. (TaggedProtocol t, ReaderM t ~ B.Get, BondType a) => BondGet t [a]
getList = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ bondTypeName t ++ " in list field, " ++ bondTypeName et ++ " expected"
    replicateM n bondGet

getVector :: forall a t. (TaggedProtocol t, ReaderM t ~ B.Get, BondType a) => BondGet t (Vector a)
getVector = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ bondTypeName t ++ " in list field, " ++ bondTypeName et ++ " expected"
    V.replicateM n bondGet

getMap :: forall k v t. (TaggedProtocol t, ReaderM t ~ B.Get, Ord k, BondType k, BondType v) => BondGet t (Map k v)
getMap = do
    let etk = getWireType (Proxy :: Proxy k)
    let etv = getWireType (Proxy :: Proxy v)
    tk <- BondDataType . fromIntegral <$> getWord8
    tv <- BondDataType . fromIntegral <$> getWord8
    unless (tk == etk) $ fail $ "invalid key tag " ++ bondTypeName tk ++ " in map field, " ++ bondTypeName etk ++ " expected"
    unless (tv == etv) $ fail $ "invalid value tag " ++ bondTypeName tv ++ " in map field, " ++ bondTypeName etv ++ " expected"
    n <- getVarInt
    fmap M.fromList $ replicateM n $ do
        k <- bondGet
        v <- bondGet
        return (k, v)

getBonded :: (TaggedProtocol t, ReaderM t ~ B.Get) => ProtocolType -> Word16 -> BondGet t (Bonded a)
getBonded sig ver = do
    size <- lookAhead $ do
        start <- bytesRead
        skipType bT_STRUCT
        stop <- bytesRead
        return (stop - start)
    bs <- getLazyByteString (fromIntegral size)
    return $ BondedStream $ BL.append (protoHeader sig ver) bs

skipVarInt :: forall t. (Protocol t, ReaderM t ~ B.Get) => BondGet t ()
skipVarInt = void (getVarInt :: BondGet t Word64)

compactSkipType :: (TaggedProtocol t, ReaderM t ~ B.Get) => BondDataType -> BondGet t ()
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
        | otherwise -> fail $ "Invalid type to skip " ++ bondTypeName t

putList :: forall a t. (TaggedProtocol t, BinaryPut (BondPutM t), BondType a) => [a] -> BondPut t
putList xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    mapM_ bondPut xs

putHashSet :: forall a t. (TaggedProtocol t, BinaryPut (BondPutM t), BondType a) => HashSet a -> BondPut t
putHashSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    mapM_ bondPut $ H.toList xs

putSet :: forall a t. (TaggedProtocol t, BinaryPut (BondPutM t), BondType a) => Set a -> BondPut t
putSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    mapM_ bondPut $ S.toList xs

putMap :: forall k v t. (Protocol t, BinaryPut (BondPutM t), BondType k, BondType v) => Map k v -> BondPut t
putMap m = do
    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        bondPut k
        bondPut v

putVector :: forall a t. (TaggedProtocol t, BinaryPut (BondPutM t), BondType a) => Vector a -> BondPut t
putVector xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    V.mapM_ bondPut xs
