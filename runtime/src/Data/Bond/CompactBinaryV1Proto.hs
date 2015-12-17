{-# Language ScopedTypeVariables, EmptyDataDecls, MultiWayIf #-}
module Data.Bond.CompactBinaryV1Proto (
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
import Control.Monad.Reader (asks)
import Data.Bits
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.ByteString as BS
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
    bondGetBlob = do
        (t, n) <- getListHeader
        unless (t == bT_INT8) $ fail $ "invalid element tag " ++ show t ++ " in blob field"
        Blob <$> getByteString n
    bondGetDefNothing = Just <$> bondGet
    bondGetList = do
        (t, n) <- getListHeader
        elemtype <- checkElementGetType t
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
        (t, n) <- getListHeader
        elemtype <- checkElementGetType t
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

    bondPutStruct = putStruct TopLevelStruct
    bondPutBaseStruct v = do
        rootT <- checkPutType bT_STRUCT
        schemaStructs <- BondPut (asks $ structs . fst)
        let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
        case base_def struct of
            Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
            Just t -> putAs t $ putStruct BaseStruct v
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

getListHeader :: BondGet CompactBinaryV1Proto (BondDataType, Int)
getListHeader = do
    t <- BondDataType . fromIntegral <$> getWord8
    n <- getVarInt
    return (t, n)

putListHeader :: (Integral a, FiniteBits a) => BondDataType -> a -> BondPut CompactBinaryV1Proto
putListHeader t n = do
    putTag t
    putVarInt n

getFieldHeader :: BondGet CompactBinaryV1Proto (BondDataType, Ordinal)
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

putFieldHeader :: BondDataType -> Ordinal -> BondPut CompactBinaryV1Proto
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

getAs :: TD.TypeDef -> BondGet CompactBinaryV1Proto a -> BondGet CompactBinaryV1Proto a
getAs typedef = glocal (\s -> s{root = typedef})

putAs :: TD.TypeDef -> BondPut CompactBinaryV1Proto -> BondPut CompactBinaryV1Proto
putAs typedef = plocal $ \(s, _) -> (s{root = typedef}, error "uninitialized cache")

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch typeSchema typeStream =
    unless (typeStream == typeSchema) $
        fail $ "Schema do not match stream: stream/struct type " ++ show typeStream ++ ", schema type " ++ show typeSchema

checkGetType :: BondDataType -> BondGet CompactBinaryV1Proto TD.TypeDef
checkGetType expected = do
    t <- BondGet $ asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: BondDataType -> BondGet CompactBinaryV1Proto TD.TypeDef
checkElementGetType expected = do
    t <- BondGet $ asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: BondDataType -> BondGet CompactBinaryV1Proto TD.TypeDef
checkKeyGetType expected = do
    t <- BondGet $ asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

getStruct :: BondStruct a => StructLevel -> BondGet CompactBinaryV1Proto a
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

skipVarInt :: BondGet CompactBinaryV1Proto ()
skipVarInt = void (getVarInt :: BondGet CompactBinaryV1Proto Word64)

skipRestOfStruct :: BondGet CompactBinaryV1Proto ()
skipRestOfStruct = skipType bT_STRUCT

skipType :: BondDataType -> BondGet CompactBinaryV1Proto ()
skipType t =
     if | t == bT_BOOL -> skip 1
        | t == bT_UINT8 -> skip 1
        | t == bT_UINT16 -> skipVarInt
        | t == bT_UINT32 -> skipVarInt
        | t == bT_UINT64 -> skipVarInt
        | t == bT_FLOAT -> skip 4
        | t == bT_DOUBLE -> skip 8
        | t == bT_STRING -> getVarInt >>= skip
        | t == bT_STRUCT -> do
            let loop = do
                    (td, _) <- getFieldHeader
                    case td of
                     _ | td == bT_STOP -> return ()
                       | td == bT_STOP_BASE -> loop
                       | otherwise -> skipType td >> loop
             in loop
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

checkPutType :: BondDataType -> BondPutM CompactBinaryV1Proto TD.TypeDef
checkPutType expected = do
    t <- BondPut $ asks (root . fst)
    checkSchemaMismatch (TD.id t) expected
    return t

checkPutContainerType :: BondDataType -> BondPutM CompactBinaryV1Proto TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: BondPutM CompactBinaryV1Proto (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)

putTag :: BondDataType -> BondPut CompactBinaryV1Proto
putTag = putWord8 . fromIntegral . fromEnum

putStruct :: BondStruct a => StructLevel -> a -> BondPut CompactBinaryV1Proto
putStruct level a = do
    t <- checkPutType bT_STRUCT
    schema <- BondPut (asks fst)
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    plocal (second (const fieldsInfo)) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putField :: forall a. BondSerializable a => Ordinal -> a -> BondPut CompactBinaryV1Proto
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

putList :: forall a. BondSerializable a => [a] -> BondPut CompactBinaryV1Proto
putList xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (length xs)
    putAs t $ mapM_ bondPut xs

putHashSet :: forall a. BondSerializable a => HashSet a -> BondPut CompactBinaryV1Proto
putHashSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (H.size xs)
    putAs t $ mapM_ bondPut $ H.toList xs

putSet :: forall a. BondSerializable a => Set a -> BondPut CompactBinaryV1Proto
putSet xs = do
    t <- checkPutContainerType bT_SET

    putListHeader (getWireType (Proxy :: Proxy a)) (S.size xs)
    putAs t $ mapM_ bondPut $ S.toList xs

putMap :: forall k v. (BondSerializable k, BondSerializable v) => Map k v -> BondPut CompactBinaryV1Proto
putMap m = do
    (tk, tv) <- checkPutMapType

    putTag $ getWireType (Proxy :: Proxy k)
    putTag $ getWireType (Proxy :: Proxy v)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        putAs tk $ bondPut k
        putAs tv $ bondPut v

putVector :: forall a. BondSerializable a => Vector a -> BondPut CompactBinaryV1Proto
putVector xs = do
    t <- checkPutContainerType bT_LIST

    putListHeader (getWireType (Proxy :: Proxy a)) (V.length xs)
    putAs t $ V.mapM_ bondPut xs
