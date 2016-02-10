{-# Language ScopedTypeVariables, TypeFamilies, FlexibleContexts, MultiWayIf #-}
module Data.Bond.SimpleBinaryProto (
        SimpleBinaryProto(..),
        SimpleBinaryV1Proto(..)
    ) where

import Data.Bond.BinaryClass
import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.Struct
import Data.Bond.Types
import Data.Bond.Utils

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.ProtocolType
import Data.Bond.Schema.SchemaDef
import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.StructDef as SD
import qualified Data.Bond.Schema.TypeDef as TD

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data SimpleBinaryProto = SimpleBinaryProto
data SimpleBinaryV1Proto = SimpleBinaryV1Proto

class Protocol t => SimpleProtocol t where
    getListHeader :: BondGet t Int
    putListHeader :: Int -> BondPut t

instance BondProto SimpleBinaryProto where
    bondRead = decode
    bondWrite = encode
    bondReadWithSchema = decodeWithSchema
    bondWriteWithSchema = encodeWithSchema
    protoSig _ = protoHeader sIMPLE_PROTOCOL 2

instance Protocol SimpleBinaryProto where
    type ReaderM SimpleBinaryProto = B.Get
    type WriterM SimpleBinaryProto = ErrorT String B.PutM

    bondGetStruct = bondStructGetUntagged
    bondGetBaseStruct = bondStructGetUntagged

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
        n <- getVarInt
        Blob <$> getByteString n
    bondGetDefNothing = Just <$> bondGet
    bondGetList = do
        n <- getVarInt
        replicateM n bondGet
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = do
        n <- getVarInt
        fmap M.fromList $ replicateM n $ liftM2 (,) bondGet bondGet
    bondGetVector = do
        n <- getVarInt
        V.replicateM n bondGet
    bondGetNullable = do
        v <- bondGetList
        case v of
            [] -> return Nothing
            [x] -> return (Just x)
            _ -> fail $ "list of length " ++ show (length v) ++ " where nullable expected"
    bondGetBonded = do
        size <- getWord32le
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream bs

    bondPutStruct = bondStructPut
    bondPutBaseStruct = bondStructPut
    bondPutField _ _ = bondPut
    bondPutDefNothingField _ _ Nothing = fail "can't save empty \"default nothing\" field with untagged protocol"
    bondPutDefNothingField _ _ (Just v) = bondPut v

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
    bondPutList xs = do
        putVarInt $ length xs
        mapM_ bondPut xs
    bondPutNullable = bondPutList . maybeToList
    bondPutHashSet = bondPutList . H.toList
    bondPutSet = bondPutList . S.toList
    bondPutMap m = do
        putVarInt $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        putVarInt $ V.length xs
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        putVarInt $ BS.length b
        putByteString b
    bondPutBonded (BondedObject _) = undefined
    bondPutBonded (BondedStream s) = do
        putWord32le $ fromIntegral $ BL.length s
        putLazyByteString s

instance SimpleProtocol SimpleBinaryProto where
    getListHeader = getVarInt
    putListHeader = putVarInt

instance BondProto SimpleBinaryV1Proto where
    bondRead = decode
    bondWrite = encode
    bondReadWithSchema = decodeWithSchema
    bondWriteWithSchema = encodeWithSchema
    protoSig _ = protoHeader sIMPLE_PROTOCOL 1

instance Protocol SimpleBinaryV1Proto where
    type ReaderM SimpleBinaryV1Proto = B.Get
    type WriterM SimpleBinaryV1Proto = ErrorT String B.PutM

    bondGetStruct = bondStructGetUntagged
    bondGetBaseStruct = bondStructGetUntagged

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
        n <- fromIntegral <$> getWord32le
        Utf8 <$> getByteString n
    bondGetWString = do
        n <- fromIntegral <$> getWord32le
        Utf16 <$> getByteString (n * 2)
    bondGetBlob = do
        n <- fromIntegral <$> getWord32le
        Blob <$> getByteString n
    bondGetDefNothing = Just <$> bondGet
    bondGetList = do
        n <- fromIntegral <$> getWord32le
        replicateM n bondGet
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = do
        n <- fromIntegral <$> getWord32le
        fmap M.fromList $ replicateM n $ liftM2 (,) bondGet bondGet
    bondGetVector = do
        n <- fromIntegral <$> getWord32le
        V.replicateM n bondGet
    bondGetNullable = do
        v <- bondGetList
        case v of
            [] -> return Nothing
            [x] -> return (Just x)
            _ -> fail $ "list of length " ++ show (length v) ++ " where nullable expected"
    bondGetBonded = do
        size <- getWord32le
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream bs

    bondPutStruct = bondStructPut
    bondPutBaseStruct = bondStructPut
    bondPutField _ _ = bondPut
    bondPutDefNothingField _ _ Nothing = fail "can't save empty \"default nothing\" field with untagged protocol"
    bondPutDefNothingField _ _ (Just v) = bondPut v

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
        putWord32le $ fromIntegral $ BS.length s
        putByteString s
    bondPutWString (Utf16 s) = do
        putWord32le $ fromIntegral $ BS.length s `div` 2
        putByteString s
    bondPutList xs = do
        putWord32le $ fromIntegral $ length xs
        mapM_ bondPut xs
    bondPutNullable = bondPutList . maybeToList
    bondPutHashSet = bondPutList . H.toList
    bondPutSet = bondPutList . S.toList
    bondPutMap m = do
        putWord32le $ fromIntegral $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        putWord32le $ fromIntegral $ V.length xs
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        putWord32le $ fromIntegral $ BS.length b
        putByteString b
    bondPutBonded (BondedObject _) = undefined
    bondPutBonded (BondedStream s) = do
        putWord32le $ fromIntegral $ BL.length s
        putLazyByteString s

instance SimpleProtocol SimpleBinaryV1Proto where
    getListHeader = fromIntegral <$> getWord32le
    putListHeader = putWord32le . fromIntegral

decode :: forall a t. (BondStruct a, Protocol t, ReaderM t ~ B.Get) => t -> BL.ByteString -> Either String a
decode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
     in case B.runGetOrFail g s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a

encode :: forall a t. (BondStruct a, Protocol t, WriterM t ~ ErrorT String B.PutM) => t -> a -> BL.ByteString
encode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
     in case tryPut g of
            Left msg -> error $ "putter returned unexpected error " ++ msg
            Right s -> s

decodeWithSchema :: forall t. (SimpleProtocol t, ReaderM t ~ B.Get) => t -> SchemaDef -> BL.ByteString -> Either String Struct
decodeWithSchema _ schema bs = do
    checkSchema schema
    case B.runGetOrFail reader bs of
        Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
        Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
        Right (_, _, a) -> Right a
    where
    BondGet reader = readStruct (root schema)
    readStruct :: TD.TypeDef -> BondGet t Struct
    readStruct td = do
        let idx = fromIntegral $ TD.struct_def td
        let sd = structs schema ! idx
        parent <- case SD.base_def sd of 
            Nothing -> return Nothing
            Just tdescr -> Just <$> readStruct tdescr
        fs <- M.fromList . V.toList <$> V.mapM readField (SD.fields sd)
        return $ Struct parent fs
    readField f = do
        value <- readValue (FD.typedef f)
        return (Ordinal $ FD.id f, value)
    readValue td
        | TD.id td == bT_BOOL = BOOL <$> bondGetBool
        | TD.id td == bT_UINT8 = UINT8 <$> bondGetUInt8
        | TD.id td == bT_UINT16 = UINT16 <$> bondGetUInt16
        | TD.id td == bT_UINT32 = UINT32 <$> bondGetUInt32
        | TD.id td == bT_UINT64 = UINT64 <$> bondGetUInt64
        | TD.id td == bT_INT8 = INT8 <$> bondGetInt8
        | TD.id td == bT_INT16 = INT16 <$> bondGetInt16
        | TD.id td == bT_INT32 = INT32 <$> bondGetInt32
        | TD.id td == bT_INT64 = INT64 <$> bondGetInt64
        | TD.id td == bT_FLOAT = FLOAT <$> bondGetFloat
        | TD.id td == bT_DOUBLE = DOUBLE <$> bondGetDouble
        | TD.id td == bT_STRING = STRING <$> bondGetString
        | TD.id td == bT_WSTRING = WSTRING <$> bondGetWString
        | TD.id td == bT_STRUCT && TD.bonded_type td = do
            n <- getWord32le
            BONDED <$> getByteString (fromIntegral n)
        | TD.id td == bT_STRUCT = STRUCT <$> readStruct td
        | TD.id td == bT_LIST = do
            let Just et = TD.element td
            n <- getListHeader
            LIST (TD.id et) <$> replicateM n (readValue et)
        | TD.id td == bT_SET = do
            let Just et = TD.element td
            n <- getListHeader
            SET (TD.id et) <$> replicateM n (readValue et)
        | TD.id td == bT_MAP = do
            let Just kt = TD.key td
            let Just vt = TD.element td
            n <- getListHeader
            fmap (MAP (TD.id kt) (TD.id vt)) $ replicateM n $ do
                k <- readValue kt
                v <- readValue vt
                return (k, v)
        | otherwise = error $ "schema validation bug: unknown field type " ++ bondTypeName (TD.id td)

encodeWithSchema :: forall t. (SimpleProtocol t, BinaryPut (WriterM t), MonadError String (WriterM t)) => t -> SchemaDef -> Struct -> Either String BL.ByteString
encodeWithSchema _ schema s = do
    struct <- checkStructSchema schema s
    let BondPut writer = putStruct (root schema) struct
    tryPut writer
    where
    putStruct :: TD.TypeDef -> Struct -> BondPut t
    putStruct td struct = do
        let idx = fromIntegral $ TD.struct_def td
        let sd = structs schema ! idx
        case (SD.base_def sd, base struct) of 
            (Nothing, Nothing) -> return ()
            (Just tdescr, Just baseStruct) -> putStruct tdescr baseStruct
            _ -> error "struct validation bug: inheritance chain in schema do not match one in struct"
        V.mapM_ (putField $ fields struct) (SD.fields sd)
    putField fieldmap f = do
        value <- maybe (mkDefault f) return $ M.lookup (Ordinal $ FD.id f) fieldmap
        putValue (FD.typedef f) value
    mkDefault f = do
        let def = default_value (FD.metadata f)
        when (nothing def) $ throwError "can't serialize default nothing with SimpleBinary protocol"
        let td = FD.typedef f
        let typ = TD.id td
        if | typ == bT_BOOL -> return $ BOOL $ uint_value def /= 0
           | typ == bT_INT8 -> return $ INT8 $ fromIntegral $ int_value def
           | typ == bT_INT16 -> return $ INT16 $ fromIntegral $ int_value def
           | typ == bT_INT32 -> return $ INT32 $ fromIntegral $ int_value def
           | typ == bT_INT64 -> return $ INT64 $ fromIntegral $ int_value def
           | typ == bT_UINT8 -> return $ UINT8 $ fromIntegral $ int_value def
           | typ == bT_UINT16 -> return $ UINT16 $ fromIntegral $ int_value def
           | typ == bT_UINT32 -> return $ UINT32 $ fromIntegral $ int_value def
           | typ == bT_UINT64 -> return $ UINT64 $ fromIntegral $ int_value def
           | typ == bT_DOUBLE -> return $ DOUBLE $ double_value def
           | typ == bT_FLOAT -> return $ FLOAT $ realToFrac $ double_value def
           | typ == bT_STRING -> return $ STRING $ string_value def
           | typ == bT_WSTRING -> return $ WSTRING $ wstring_value def
           | typ == bT_LIST -> return $ LIST (TD.id $ fromJust $ TD.element td) []
           | typ == bT_SET -> return $ SET (TD.id $ fromJust $ TD.element td) []
           | typ == bT_MAP -> return $ MAP (TD.id $ fromJust $ TD.key td) (TD.id $ fromJust $ TD.element td) []
           | otherwise -> throwError $ "can't make default value for type " ++ bondTypeName typ ++ ", struct must have it"
    putValue _ (BOOL b) = bondPutBool b
    putValue _ (INT8 v) = bondPutInt8 v
    putValue _ (INT16 v) = bondPutInt16 v
    putValue _ (INT32 v) = bondPutInt32 v
    putValue _ (INT64 v) = bondPutInt64 v
    putValue _ (UINT8 v) = bondPutUInt8 v
    putValue _ (UINT16 v) = bondPutUInt16 v
    putValue _ (UINT32 v) = bondPutUInt32 v
    putValue _ (UINT64 v) = bondPutUInt64 v
    putValue _ (FLOAT v) = bondPutFloat v
    putValue _ (DOUBLE v) = bondPutDouble v
    putValue _ (STRING v) = bondPutString v
    putValue _ (WSTRING v) = bondPutWString v
    putValue td (STRUCT v) | TD.bonded_type td = throwError "bonded not implemented"
    putValue td (STRUCT v) = putStruct td v
    putValue td (LIST _ xs) = do
        let Just etd = TD.element td
        putListHeader $ length xs
        mapM_ (putValue etd) xs
    putValue td (SET _ xs) = do
        let Just etd = TD.element td
        putListHeader $ length xs
        mapM_ (putValue etd) xs
    putValue td (MAP _ _ xs) = do
        let Just vtd = TD.element td
        let Just ktd = TD.key td
        putListHeader $ length xs
        forM_ xs $ \ (k, v) -> putValue ktd k >> putValue vtd v
    putValue _ (BONDED stream) = do
        putWord32le $ fromIntegral $ BS.length stream
        putByteString stream
