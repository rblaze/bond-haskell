{-# Language ScopedTypeVariables, EmptyDataDecls, MultiWayIf #-}
module Data.Bond.CompactBinaryProto (
        CompactBinaryProto,
        CompactBinaryV1Proto
    ) where

import Data.Bond.Cast
import Data.Bond.Default
import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Wire

import Control.Applicative hiding (optional)
import Control.Arrow ((&&&), second)
import Control.Monad
import Control.Monad.Reader (runReaderT, ask, asks)
import Data.Binary.Put (runPut)
import Data.Bits
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.TypeDef as TD
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.StructDef

data CompactBinaryProto
data CompactBinaryV1Proto

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

newtype ZigZagInt = ZigZagInt { fromZigZag :: Int64 }
    deriving (Show, Eq)

zigzagToWord :: ZigZagInt -> Word64
zigzagToWord (ZigZagInt i) | i >= 0 = 2 * fromIntegral i
zigzagToWord (ZigZagInt i) = (2 * fromIntegral (abs i)) - 1

wordToZigZag :: Word64 -> ZigZagInt
wordToZigZag w | even w = ZigZagInt $ fromIntegral (w `div` 2)
wordToZigZag w = ZigZagInt $ negate $ fromIntegral ((w - 1) `div` 2) + 1

class BondProto t => ProtocolSpecific t where
    getListHeader :: BondGet t (BondDataType, Int)
    putListHeader :: (Integral a, FiniteBits a) => BondDataType -> a -> BondPut t
    skipStruct :: BondGet t ()
    skipRestOfStruct :: BondGet t ()

instance ProtocolSpecific CompactBinaryProto where
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
    skipStruct = getVarInt >>= skip
    skipRestOfStruct =
        let loop = do
                (wiretype, _) <- getFieldHeader
                if | wiretype == bT_STOP -> return ()
                   | wiretype == bT_STOP_BASE -> loop
                   | otherwise -> skipType wiretype >> loop
         in loop

instance BondProto CompactBinaryProto where
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
    bondGetInt16 = fromIntegral . fromZigZag . wordToZigZag <$> getVarInt
    bondGetInt32 = fromIntegral . fromZigZag . wordToZigZag <$> getVarInt
    bondGetInt64 = fromZigZag . wordToZigZag <$> getVarInt
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
    bondGetBonded = do
        size <- getVarInt
        bs <- getLazyByteString size -- FIXME prepend length to keep correct format
        return $ BondedStream bs

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
    bondPutInt16 = putVarInt . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt32 = putVarInt . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt64 = putVarInt . zigzagToWord . ZigZagInt
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
    bondPutBonded (BondedStream s) = do
        putVarInt (BL.length s)
        putLazyByteString s -- FIXME handle different protocols

instance ProtocolSpecific CompactBinaryV1Proto where
    getListHeader = do
        t <- BondDataType . fromIntegral <$> getWord8
        n <- getVarInt
        return (t, n)
    putListHeader t n = do
        putTag t
        putVarInt n
    skipStruct =
        let loop = do
                (td, _) <- getFieldHeader
                if | td == bT_STOP -> return ()
                   | td == bT_STOP_BASE -> loop
                   | otherwise -> skipType td >> loop
         in loop
    skipRestOfStruct = skipType bT_STRUCT

instance BondProto CompactBinaryV1Proto where
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
    bondGetInt16 = fromIntegral . fromZigZag . wordToZigZag <$> getVarInt
    bondGetInt32 = fromIntegral . fromZigZag . wordToZigZag <$> getVarInt
    bondGetInt64 = fromZigZag . wordToZigZag <$> getVarInt
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
    bondGetBonded = do
        size <- lookAhead $ do
            start <- bytesRead
            skipType bT_STRUCT
            stop <- bytesRead
            return (stop - start)
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream bs

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
    bondPutInt16 = putVarInt . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt32 = putVarInt . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt64 = putVarInt . zigzagToWord . ZigZagInt
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

getFieldHeader :: BondGet t (BondDataType, Ordinal)
getFieldHeader = do
    tag <- getWord8
    case tag `shiftR` 5 of
        6 -> do
            n <- getWord8
            return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal (fromIntegral n))
        7 -> do
            n <- getWord16le
            return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal n)
        n -> return (BondDataType $ fromIntegral $ tag .&. 31, Ordinal (fromIntegral n))

putFieldHeader :: BondDataType -> Ordinal -> BondPut t
putFieldHeader t (Ordinal n) =
    let tbits = fromIntegral $ fromEnum t
        nbits = fromIntegral n
     in if | n <= 5 -> putWord8 $ tbits .|. (nbits `shiftL` 5)
           | n <= 255 -> do
                    putWord8 $ tbits .|. 0xC0
                    putWord8 nbits
           | otherwise -> do
                    putWord8 $ tbits .|. 0xE0
                    putWord16le n

getAs :: BondProto t => TD.TypeDef -> BondGet t a -> BondGet t a
getAs typedef = glocal (\s -> s{root = typedef})

putAs :: BondProto t => TD.TypeDef -> BondPut t -> BondPut t
putAs typedef = plocal $ \(s, _) -> (s{root = typedef}, error "uninitialized cache")

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch typeSchema typeStream =
    unless (typeStream == typeSchema) $
        fail $ "Schema do not match stream: stream/struct type " ++ show typeStream ++ ", schema type " ++ show typeSchema

checkGetType :: BondDataType -> BondGet t TD.TypeDef
checkGetType expected = do
    t <- BondGet $ asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: BondDataType -> BondGet t TD.TypeDef
checkElementGetType expected = do
    t <- BondGet $ asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: BondDataType -> BondGet t TD.TypeDef
checkKeyGetType expected = do
    t <- BondGet $ asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

getStruct :: (ProtocolSpecific t, BondStruct a) => StructLevel -> BondGet t a
getStruct level = do
    rootT <- checkGetType bT_STRUCT
    schemaStructs <- BondGet (asks structs)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    base <- case base_def struct of
        Nothing -> return defaultValue
        Just t -> getAs t (bondStructGetBase defaultValue)
    -- iterate over stream, update fields
    let fieldTypes = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& FD.typedef) $ fields struct
    let readField wiretype ordinal s =
            case M.lookup ordinal fieldTypes of
                Nothing -> do
                    skipType wiretype -- unknown field, ignore it
                    return s
                Just t -> do
                    checkSchemaMismatch (TD.id t) wiretype
                    getAs t $ bondStructGetField ordinal s
    let loop s = do
            (wiretype, ordinal) <- getFieldHeader
            if | wiretype == bT_STOP && level == BaseStruct -> fail "BT_STOP found where BT_STOP_BASE expected"
               | wiretype == bT_STOP && level == TopLevelStruct -> return s
               | wiretype == bT_STOP_BASE && level == BaseStruct -> return s
               | wiretype == bT_STOP_BASE && level == TopLevelStruct -> skipRestOfStruct >> return s
               | otherwise -> readField wiretype ordinal s >>= loop

    loop base

getBlob :: ProtocolSpecific t => BondGet t Blob
getBlob = do
    (t, n) <- getListHeader
    unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
    Blob <$> getByteString n

getList :: (ProtocolSpecific t, BondSerializable a) => BondGet t [a]
getList = do
    (t, n) <- getListHeader
    elemtype <- checkElementGetType t
    getAs elemtype $ replicateM n bondGet

getVector :: (ProtocolSpecific t, BondSerializable a) => BondGet t (Vector a)
getVector = do
    (t, n) <- getListHeader
    elemtype <- checkElementGetType t
    getAs elemtype $ V.replicateM n bondGet

getMap :: (ProtocolSpecific t, Ord k, BondSerializable k, BondSerializable v) => BondGet t (Map k v)
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

skipVarInt :: forall t. BondGet t ()
skipVarInt = void (getVarInt :: BondGet t Word64)

skipType :: ProtocolSpecific t => BondDataType -> BondGet t ()
skipType t =
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

checkPutType :: BondDataType -> BondPutM t TD.TypeDef
checkPutType expected = do
    t <- BondPut $ asks (root . fst)
    checkSchemaMismatch (TD.id t) expected
    return t

checkPutContainerType :: BondDataType -> BondPutM t TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: BondPutM t (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)

putTag :: BondDataType -> BondPut t
putTag = putWord8 . fromIntegral . fromEnum

putStruct :: (BondProto t, BondStruct a) => StructLevel -> a -> BondPut t
putStruct level a = do
    t <- checkPutType bT_STRUCT
    schema <- BondPut (asks fst)
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    plocal (second (const fieldsInfo)) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (BondProto t, BondStruct a) => a -> BondPut t
putBaseStruct v = do
    rootT <- checkPutType bT_STRUCT
    schemaStructs <- BondPut (asks $ structs . fst)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    case base_def struct of
        Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
        Just t -> putAs t $ putStruct BaseStruct v

putField :: forall a t. (BondProto t, BondSerializable a) => Ordinal -> a -> BondPut t
putField n a = do
    fieldTypes <- BondPut (asks snd)
    let Just f = M.lookup n fieldTypes
    let t = FD.typedef f
    let tag = getWireType (Proxy :: Proxy a)
    checkSchemaMismatch (TD.id t) tag

    let needToSave = not (equalToDefault (default_value $ FD.metadata f) a) ||
           modifier (FD.metadata f) /= optional
    when needToSave $ do
        putFieldHeader tag n
        putAs t $ bondPut a

putList :: forall a t. (ProtocolSpecific t, BondSerializable a) => [a] -> BondPut t
putList xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    putAs t $ mapM_ bondPut xs

putHashSet :: forall a t. (ProtocolSpecific t, BondSerializable a) => HashSet a -> BondPut t
putHashSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    putAs t $ mapM_ bondPut $ H.toList xs

putSet :: forall a t. (ProtocolSpecific t, BondSerializable a) => Set a -> BondPut t
putSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    putAs t $ mapM_ bondPut $ S.toList xs

putMap :: forall k v t. (BondProto t, BondSerializable k, BondSerializable v) => Map k v -> BondPut t
putMap m = do
    (tk, tv) <- checkPutMapType

    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        putAs tk $ bondPut k
        putAs tv $ bondPut v

putVector :: forall a t. (ProtocolSpecific t, BondSerializable a) => Vector a -> BondPut t
putVector xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    putAs t $ V.mapM_ bondPut xs
