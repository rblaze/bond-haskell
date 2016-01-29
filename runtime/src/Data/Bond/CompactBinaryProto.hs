{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, ConstraintKinds #-}
module Data.Bond.CompactBinaryProto (
        CompactBinaryProto(..),
        CompactBinaryV1Proto(..)
    ) where

import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.TaggedProtocol
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Wire
import Data.Bond.ZigZag

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.ProtocolType

import Control.Applicative hiding (optional)
import Control.Monad
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
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.Vector as V

data CompactBinaryProto = CompactBinaryProto
data CompactBinaryV1Proto = CompactBinaryV1Proto

class TaggedProtocol t => CompactProtocol t where
    getListHeader :: ReaderM t ~ B.Get => BondGet t (BondDataType, Int)
    putListHeader :: (Integral a, FiniteBits a, WriterM t ~ B.PutM) => BondDataType -> a -> BondPut t
    readStruct :: ReaderM t ~ B.Get => BondGet t Struct
    writeTopLevelStruct :: WriterM t ~ B.PutM => Struct -> BondPut t

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
    readStruct = do
        size <- getVarInt
        isolate size readStructData
    writeTopLevelStruct v = do
        let BondPut g = writeStructData v >> putTag bT_STOP :: BondPut CompactBinaryProto
        let bs = runPut g
        putVarInt $ BL.length bs
        putLazyByteString bs

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
    bondRead = binaryDecode
    bondWrite = binaryEncode
    protoSig _ = protoHeader cOMPACT_PROTOCOL 2

instance BondTaggedProto CompactBinaryProto where
    bondReadTagged _ = readSchemaless CompactBinaryProto
    bondWriteTagged _ = writeSchemaless CompactBinaryProto

instance Protocol CompactBinaryProto where
    type ReaderM CompactBinaryProto = B.Get
    type WriterM CompactBinaryProto = B.PutM

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
        let bs = runPut g
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
    bondPutBonded (BondedStream s) = putLazyByteString (BL.drop 4 s) -- FIXME handle different protocols

instance CompactProtocol CompactBinaryV1Proto where
    getListHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- getVarInt
        return (t, n)
    putListHeader t n = do
        putTag t
        putVarInt n
    readStruct = readStructData
    writeTopLevelStruct s = writeStructData s >> putTag bT_STOP

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
    bondRead = binaryDecode
    bondWrite = binaryEncode
    protoSig _ = protoHeader cOMPACT_PROTOCOL 1

instance BondTaggedProto CompactBinaryV1Proto where
    bondReadTagged _ = readSchemaless CompactBinaryV1Proto
    bondWriteTagged _ = writeSchemaless CompactBinaryV1Proto

instance Protocol CompactBinaryV1Proto where
    type ReaderM CompactBinaryV1Proto = B.Get
    type WriterM CompactBinaryV1Proto = B.PutM

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
    bondPutBonded (BondedStream s) = putLazyByteString (BL.drop 4 s) -- FIXME handle different protocols

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

putCompactFieldHeader :: (BondProto t, WriterM t ~ B.PutM) => BondDataType -> Ordinal -> BondPut t
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

getBlob :: (CompactProtocol t, ReaderM t ~ B.Get) => BondGet t Blob
getBlob = do
    (t, n) <- getListHeader
    unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
    Blob <$> getByteString n

getList :: forall a t. (CompactProtocol t, ReaderM t ~ B.Get, Serializable a) => BondGet t [a]
getList = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    replicateM n bondGet

getVector :: forall a t. (CompactProtocol t, ReaderM t ~ B.Get, Serializable a) => BondGet t (Vector a)
getVector = do
    let et = getWireType (Proxy :: Proxy a)
    (t, n) <- getListHeader
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    V.replicateM n bondGet

getMap :: forall k v t. (TaggedProtocol t, ReaderM t ~ B.Get, Ord k, Serializable k, Serializable v) => BondGet t (Map k v)
getMap = do
    let etk = getWireType (Proxy :: Proxy k)
    let etv = getWireType (Proxy :: Proxy v)
    tk <- BondDataType . fromIntegral <$> getWord8
    tv <- BondDataType . fromIntegral <$> getWord8
    unless (tk == etk) $ fail $ "invalid key tag " ++ show tk ++ " in map field, " ++ show etk ++ " expected"
    unless (tv == etv) $ fail $ "invalid value tag " ++ show tv ++ " in map field, " ++ show etv ++ " expected"
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

compactSkipType :: (CompactProtocol t, ReaderM t ~ B.Get) => BondDataType -> BondGet t ()
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

putList :: forall a t. (CompactProtocol t, WriterM t ~ B.PutM, Serializable a) => [a] -> BondPut t
putList xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    mapM_ bondPut xs

putHashSet :: forall a t. (CompactProtocol t, WriterM t ~ B.PutM, Serializable a) => HashSet a -> BondPut t
putHashSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    mapM_ bondPut $ H.toList xs

putSet :: forall a t. (CompactProtocol t, WriterM t ~ B.PutM, Serializable a) => Set a -> BondPut t
putSet xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    mapM_ bondPut $ S.toList xs

putMap :: forall k v t. (Protocol t, WriterM t ~ B.PutM, Serializable k, Serializable v) => Map k v -> BondPut t
putMap m = do
    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        bondPut k
        bondPut v

putVector :: forall a t. (CompactProtocol t, WriterM t ~ B.PutM, Serializable a) => Vector a -> BondPut t
putVector xs = do
    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    V.mapM_ bondPut xs

readStructData :: forall t. (ReaderM t ~ B.Get, CompactProtocol t) => BondGet t Struct
readStructData = fieldLoop $ Struct Nothing M.empty
    where
    getValue :: BondDataType -> BondGet t Value
    getValue t =
        if | t == bT_BOOL -> BOOL <$> bondGetBool
           | t == bT_UINT8 -> UINT8 <$> bondGetUInt8
           | t == bT_UINT16 -> UINT16 <$> bondGetUInt16
           | t == bT_UINT32 -> UINT32 <$> bondGetUInt32
           | t == bT_UINT64 -> UINT64 <$> bondGetUInt64
           | t == bT_FLOAT -> FLOAT <$> bondGetFloat
           | t == bT_DOUBLE -> DOUBLE <$> bondGetDouble
           | t == bT_STRING -> STRING <$> bondGetString
           | t == bT_STRUCT -> STRUCT <$> readStruct
           | t == bT_LIST -> do
                (td, n) <- getListHeader
                LIST td <$> replicateM n (getValue td)
           | t == bT_SET -> do
                (td, n) <- getListHeader
                SET td <$> replicateM n (getValue td)
           | t == bT_MAP -> do
                tk <- BondDataType . fromIntegral <$> getWord8
                tv <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
                MAP tk tv <$> replicateM n (do
                    k <- getValue tk
                    v <- getValue tv
                    return (k, v))
           | t == bT_INT8 -> INT8 <$> bondGetInt8
           | t == bT_INT16 -> INT16 <$> bondGetInt16
           | t == bT_INT32 -> INT32 <$> bondGetInt32
           | t == bT_INT64 -> INT64 <$> bondGetInt64
           | t == bT_WSTRING -> WSTRING <$> bondGetWString
           | otherwise -> fail $ "invalid field type " ++ show t
    setField s o v = return $ s { fields = MS.insert o v (fields s) }
    fieldLoop s = do
        (t, o) <- getFieldHeader
        if | t == bT_STOP -> return s
           | t == bT_STOP_BASE -> fieldLoop $ Struct (Just s) M.empty
           | otherwise -> getValue t >>= setField s o >>= fieldLoop

readSchemaless :: forall t. (ReaderM t ~ B.Get, CompactProtocol t) => t -> BL.ByteString -> Either String Struct
readSchemaless _ stream
    = let BondGet g = readStruct :: BondGet t Struct
       in case B.runGetOrFail g stream of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a

writeStructData :: forall t. (WriterM t ~ B.PutM, CompactProtocol t) => Struct -> BondPut t
writeStructData s = do
    case base s of
        Just b -> writeStructData b >> putTag bT_STOP_BASE
        Nothing -> return ()
    forM_ (M.toList $ fields s) $ \ (o, v) -> do
        let (typ, writer) = saveValue v
        putFieldHeader typ o
        writer
    where
    saveValue :: Value -> (BondDataType, BondPut t)
    saveValue (BOOL v) = (bT_BOOL, bondPutBool v)
    saveValue (INT8 v) = (bT_INT8, bondPutInt8 v)
    saveValue (INT16 v) = (bT_INT16, bondPutInt16 v)
    saveValue (INT32 v) = (bT_INT32, bondPutInt32 v)
    saveValue (INT64 v) = (bT_INT64, bondPutInt64 v)
    saveValue (UINT8 v) = (bT_UINT8, bondPutUInt8 v)
    saveValue (UINT16 v) = (bT_UINT16, bondPutUInt16 v)
    saveValue (UINT32 v) = (bT_UINT32, bondPutUInt32 v)
    saveValue (UINT64 v) = (bT_UINT64, bondPutUInt64 v)
    saveValue (FLOAT v) = (bT_FLOAT, bondPutFloat v)
    saveValue (DOUBLE v) = (bT_DOUBLE, bondPutDouble v)
    saveValue (STRING v) = (bT_STRING, bondPutString v)
    saveValue (WSTRING v) = (bT_WSTRING, bondPutWString v)
    saveValue (STRUCT v) = (bT_STRUCT, writeTopLevelStruct v)
    saveValue (LIST td xs) = (bT_LIST, putListHeader td (length xs) >> mapM_ (snd . saveValue) xs)
    saveValue (SET td xs) = (bT_SET, putListHeader td (length xs) >> mapM_ (snd . saveValue) xs)
    saveValue (MAP tk tv xs) = (bT_MAP, do
        putTag tk
        putTag tv
        putVarInt $ length xs
        forM_ xs $ \ (k, v) -> do
            snd $ saveValue k
            snd $ saveValue v
      )

writeSchemaless :: forall t. (WriterM t ~ B.PutM, CompactProtocol t) => t -> Struct -> BL.ByteString
writeSchemaless _ s = let BondPut g = writeTopLevelStruct s :: BondPut t
                       in runPut g
