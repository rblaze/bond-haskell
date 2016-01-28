{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies #-}
module Data.Bond.FastBinaryProto (
        FastBinaryProto(..)
    ) where

import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.Struct
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
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.Vector as V

data FastBinaryProto = FastBinaryProto

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
    bondRead = binaryDecode
    bondWrite = binaryEncode
    protoSig _ = protoHeader fAST_PROTOCOL 1

instance BondTaggedProto FastBinaryProto where
    bondReadTagged _ = readSchemaless
    bondWriteTagged _ = writeSchemaless

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
        t <- BondDataType . fromIntegral <$> getWord8
        unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
        n <- getVarInt
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
    t <- BondDataType . fromIntegral <$> getWord8
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    n <- getVarInt
    replicateM n bondGet

getVector :: forall a. Serializable a => BondGet FastBinaryProto (Vector a)
getVector = do
    let et = getWireType (Proxy :: Proxy a)
    t <- BondDataType . fromIntegral <$> getWord8
    unless (t == et) $ fail $ "invalid element tag " ++ show t ++ " in list field, " ++ show et ++ " expected"
    n <- getVarInt
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
    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ length xs
    mapM_ bondPut xs

putHashSet :: forall a. Serializable a => HashSet a -> BondPut FastBinaryProto
putHashSet xs = do
    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ H.size xs
    mapM_ bondPut $ H.toList xs

putSet :: forall a. Serializable a => Set a -> BondPut FastBinaryProto
putSet xs = do
    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ S.size xs
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
    putTag $ getWireType (Proxy :: Proxy a)
    putVarInt $ V.length xs
    V.mapM_ bondPut xs

readSchemaless :: BL.ByteString -> Either String Struct
readSchemaless stream
    = let BondGet g = readStruct
       in case B.runGetOrFail g stream of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
--            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a
    where
    getValue :: BondDataType -> BondGet FastBinaryProto Value
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
                td <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
                LIST td <$> replicateM n (getValue td)
           | t == bT_SET -> do
                td <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
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
    readStruct = fieldLoop $ Struct Nothing M.empty

writeSchemaless :: Struct -> BL.ByteString
writeSchemaless struct = let BondPut g = writeStruct TopLevelStruct struct in B.runPut g
    where
    writeStruct level s = do
        case base s of
            Just b -> writeStruct BaseStruct b
            Nothing -> return ()
        forM_ (M.toList $ fields s) $ \ (o, v) -> do
            let (typ, writer) = saveValue v
            putFieldHeader typ o
            writer
        case level of
            TopLevelStruct -> putTag bT_STOP
            BaseStruct -> putTag bT_STOP_BASE
    saveValue :: Value -> (BondDataType, BondPut FastBinaryProto)
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
    saveValue (STRUCT v) = (bT_STRUCT, writeStruct TopLevelStruct v)
    saveValue (LIST td xs) = (bT_LIST, do { putTag td; putVarInt $ length xs; mapM_ (snd . saveValue) xs })
    saveValue (SET td xs) = (bT_SET, do { putTag td; putVarInt $ length xs; mapM_ (snd . saveValue) xs })
    saveValue (MAP tk tv xs) = (bT_MAP, do
        putTag tk
        putTag tv
        putVarInt $ length xs
        forM_ xs $ \ (k, v) -> do
            snd $ saveValue k
            snd $ saveValue v
      )
