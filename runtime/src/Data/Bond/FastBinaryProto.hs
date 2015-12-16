{-# Language ScopedTypeVariables, EmptyDataDecls, MultiWayIf #-}
module Data.Bond.FastBinaryProto (
        FastBinaryProto
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

data FastBinaryProto

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

instance BondProto FastBinaryProto where
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
    bondGetVector = V.fromList <$> bondGetList
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

getAs :: TD.TypeDef -> BondGet FastBinaryProto a -> BondGet FastBinaryProto a
getAs typedef = glocal (\s -> s{root = typedef})

putAs :: TD.TypeDef -> BondPut FastBinaryProto -> BondPut FastBinaryProto
putAs typedef = plocal $ \(s, _) -> (s{root = typedef}, error "uninitialized cache")

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch typeSchema typeStream =
    unless (typeStream == typeSchema) $
        fail $ "Schema do not match stream: stream/struct type " ++ show typeStream ++ ", schema type " ++ show typeSchema

checkGetType :: BondDataType -> BondGet FastBinaryProto TD.TypeDef
checkGetType expected = do
    t <- BondGet $ asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: BondDataType -> BondGet FastBinaryProto TD.TypeDef
checkElementGetType expected = do
    t <- BondGet $ asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: BondDataType -> BondGet FastBinaryProto TD.TypeDef
checkKeyGetType expected = do
    t <- BondGet $ asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

getStruct :: BondStruct a => StructLevel -> BondGet FastBinaryProto a
getStruct level = do
    rootT <- checkGetType bT_STRUCT
    schemaStructs <- BondGet (asks structs)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    base <- case base_def struct of
        Nothing -> return defaultValue
        Just t -> getAs t (bondStructGetBase defaultValue)
    -- iterate over stream, update fields
    let fieldTypes = M.fromList $ V.toList $ V.map (FD.id &&& FD.typedef) $ fields struct
    let readField wiretype s = do
            ordinal <- getWord16le
            case M.lookup ordinal fieldTypes of
                Nothing -> do
                    skipType wiretype -- unknown field, ignore it
                    return s
                Just t -> do
                    checkSchemaMismatch (TD.id t) wiretype
                    getAs t $ bondStructGetField (Ordinal ordinal) s
    let loop s = do
            wiretype <- BondDataType . fromIntegral <$> getWord8
            if | wiretype == bT_STOP && level == BaseStruct -> fail "BT_STOP found where BT_STOP_BASE expected"
               | wiretype == bT_STOP && level == TopLevelStruct -> return s
               | wiretype == bT_STOP_BASE && level == BaseStruct -> return s
               | wiretype == bT_STOP_BASE && level == TopLevelStruct -> skipType bT_STRUCT >> return s
               | otherwise -> readField wiretype s >>= loop

    loop base

skipType :: BondDataType -> BondGet FastBinaryProto ()
skipType t = case True of
     _ | t == bT_BOOL -> skip 1
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
                    case td of
                     _ | td == bT_STOP -> return ()
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
       | otherwise -> error $ "Invalid type to skip " ++ show t

checkPutType :: BondDataType -> BondPutM FastBinaryProto TD.TypeDef
checkPutType expected = do
    t <- BondPut $ asks (root . fst)
    checkSchemaMismatch (TD.id t) expected
    return t

checkPutContainerType :: BondDataType -> BondPutM FastBinaryProto TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: BondPutM FastBinaryProto (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)

putTag :: BondDataType -> BondPut FastBinaryProto
putTag = putWord8 . fromIntegral . fromEnum

putStruct :: BondStruct a => StructLevel -> a -> BondPut FastBinaryProto
putStruct level a = do
    t <- checkPutType bT_STRUCT
    schema <- BondPut (asks fst)
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    plocal (second (const fieldsInfo)) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putField :: forall a. BondSerializable a => Ordinal -> a -> BondPut FastBinaryProto
putField n a = do
    fieldTypes <- BondPut (asks snd)
    let Just f = M.lookup n fieldTypes
    let t = FD.typedef f
    let tag = getWireType (Proxy :: Proxy a)
    checkSchemaMismatch (TD.id t) tag

    let needToSave = not (equalToDefault (default_value $ FD.metadata f) a) ||
           modifier (FD.metadata f) /= optional
    when needToSave $ do
        putTag tag
        let Ordinal fn = n in putWord16le fn
        putAs t $ bondPut a

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
