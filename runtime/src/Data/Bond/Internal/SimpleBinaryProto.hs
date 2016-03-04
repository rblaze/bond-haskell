{-# Language ScopedTypeVariables, TypeFamilies, FlexibleContexts, MultiWayIf #-}
module Data.Bond.Internal.SimpleBinaryProto (
        SimpleBinaryProto(..),
        SimpleBinaryV1Proto(..)
    ) where

import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.TypedSchema
import Data.Bond.Types
import Data.Bond.Internal.BinaryUtils
import Data.Bond.Internal.Cast
import Data.Bond.Internal.CompactBinaryProto
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.ProtoUtils
import Data.Bond.Internal.SchemaOps
import Data.Bond.Internal.SchemaUtils

import Data.Bond.Schema.ProtocolType

import Control.Applicative
import Control.Monad.Error
import Data.List
import Data.Maybe
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Traversable as T -- XXX lts-2
import qualified Data.Vector as V

{-|
A binary, untagged protocol which is a good choice for storage scenarios as it offers potential for big saving on payload size. Because Simple is an untagged protocol, it requires that the payload schema is available during deserialization.
Version 2 of Simple Protocol uses variable integer encoding for string and container lengths, resulting in more compact payload.
-}
data SimpleBinaryProto = SimpleBinaryProto
-- |A binary, untagged protocol which is a good choice for storage scenarios as it offers potential for big saving on payload size. Because Simple is an untagged protocol, it requires that the payload schema is available during deserialization.
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
    bondPutDefNothingField _ _ Nothing = throwError "can't save empty \"default nothing\" field with untagged protocol"
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
    bondPutBonded (BondedObject v) = do
        stream <- either throwError return $ bondMarshal CompactBinaryProto v
        putWord32le $ fromIntegral $ BL.length stream
        putLazyByteString stream
    bondPutBonded (BondedStream stream) = do
        putWord32le $ fromIntegral $ BL.length stream
        putLazyByteString stream

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
    bondPutDefNothingField _ _ Nothing = throwError "can't save empty \"default nothing\" field with untagged protocol"
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
    bondPutBonded (BondedObject v) = do
        stream <- either throwError return $ bondMarshal CompactBinaryProto v
        putWord32le $ fromIntegral $ BL.length stream
        putLazyByteString stream
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

encode :: forall a t. (BondStruct a, Protocol t, WriterM t ~ ErrorT String B.PutM) => t -> a -> Either String BL.ByteString
encode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
     in tryPut g

decodeWithSchema :: forall t. (SimpleProtocol t, ReaderM t ~ B.Get) => t -> StructSchema -> BL.ByteString -> Either String Struct
decodeWithSchema _ rootSchema bs =
    case B.runGetOrFail reader bs of
        Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
        Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
        Right (_, _, a) -> Right a
    where
    BondGet reader = readStruct rootSchema
    readStruct :: StructSchema -> BondGet t Struct
    readStruct schema = do
        parent <- case structBase schema of
            Nothing -> return Nothing
            Just baseSchema -> Just <$> readStruct baseSchema
        fs <- T.mapM (readField . fieldType) (structFields schema)
        return $ Struct parent fs
    readField = readValue . fieldToElementType

    readValue ElementBool = BOOL <$> bondGetBool
    readValue ElementUInt8 = UINT8 <$> bondGetUInt8
    readValue ElementUInt16 = UINT16 <$> bondGetUInt16
    readValue ElementUInt32 = UINT32 <$> bondGetUInt32
    readValue ElementUInt64 = UINT64 <$> bondGetUInt64
    readValue ElementInt8 = INT8 <$> bondGetInt8
    readValue ElementInt16 = INT16 <$> bondGetInt16
    readValue ElementInt32 = INT32 <$> bondGetInt32
    readValue ElementInt64 = INT64 <$> bondGetInt64
    readValue ElementFloat = FLOAT <$> bondGetFloat
    readValue ElementDouble = DOUBLE <$> bondGetDouble
    readValue ElementString = STRING <$> bondGetString
    readValue ElementWString = WSTRING <$> bondGetWString
    readValue (ElementBonded _) = do
        n <- getWord32le
        BONDED . BondedStream <$> getLazyByteString (fromIntegral n)
    readValue (ElementStruct schema) = STRUCT <$> readStruct schema
    readValue (ElementList element) = do
        n <- getListHeader
        LIST (elementToBondDataType element) <$> replicateM n (readValue element)
    readValue (ElementSet element) = do
        n <- getListHeader
        SET (elementToBondDataType element) <$> replicateM n (readValue element)
    readValue (ElementMap key value) = do
        n <- getListHeader
        fmap (MAP (elementToBondDataType key) (elementToBondDataType value)) $ replicateM n $ do
            k <- readValue key
            v <- readValue value
            return (k, v)

encodeWithSchema :: forall t. (SimpleProtocol t, WriterM t ~ ErrorT String B.PutM) => t -> StructSchema -> Struct -> Either String BL.ByteString
encodeWithSchema _ rootSchema s = do
    struct <- checkStructSchema rootSchema s
    let BondPut writer = putStruct rootSchema struct
    tryPut writer
    where
    putStruct :: StructSchema -> Struct -> BondPut t
    putStruct schema struct = do
        case (structBase schema, base struct) of 
            (Nothing, Nothing) -> return ()
            (Just baseSchema, Just baseStruct) -> putStruct baseSchema baseStruct
            _ -> error "internal error: inheritance chain in schema do not match one in struct"
        mapM_ (putField $ fields struct) $ M.toAscList $ structFields schema
    putField fieldmap (fieldId, fieldInfo) = do
        value <- maybe (getDefault $ fieldType fieldInfo) return $ M.lookup fieldId fieldmap
        putValue (fieldToElementType $ fieldType fieldInfo) value

    getDefault = maybe (throwError "can't serialize default nothing with SimpleBinary protocol") return . defaultFieldValue

    putValue ElementBool (BOOL b) = bondPutBool b
    putValue ElementInt8 (INT8 v) = bondPutInt8 v
    putValue ElementInt16 (INT16 v) = bondPutInt16 v
    putValue ElementInt32 (INT32 v) = bondPutInt32 v
    putValue ElementInt64 (INT64 v) = bondPutInt64 v
    putValue ElementUInt8 (UINT8 v) = bondPutUInt8 v
    putValue ElementUInt16 (UINT16 v) = bondPutUInt16 v
    putValue ElementUInt32 (UINT32 v) = bondPutUInt32 v
    putValue ElementUInt64 (UINT64 v) = bondPutUInt64 v
    putValue ElementFloat (FLOAT v) = bondPutFloat v
    putValue ElementDouble (DOUBLE v) = bondPutDouble v
    putValue ElementString (STRING v) = bondPutString v
    putValue ElementWString (WSTRING v) = bondPutWString v
    putValue (ElementStruct schema) (STRUCT v) = putStruct schema v
    putValue (ElementList element) (LIST _ xs) = do
        putListHeader $ length xs
        mapM_ (putValue element) xs
    putValue (ElementSet element) (SET _ xs) = do
        putListHeader $ length xs
        mapM_ (putValue element) xs
    putValue (ElementMap key value) (MAP _ _ xs) = do
        putListHeader $ length xs
        forM_ xs $ \ (k, v) -> putValue key k >> putValue value v
    putValue ElementBonded{} (BONDED (BondedStream stream)) = do
        putWord32le $ fromIntegral $ BL.length stream
        putLazyByteString stream
    putValue (ElementBonded schema) (BONDED (BondedObject struct)) = do
        stream <- either throwError return $ bondMarshalWithSchema CompactBinaryProto schema struct
        putWord32le $ fromIntegral $ BL.length stream
        putLazyByteString stream
    putValue _ _ = error "internal error: schema type do not match value type"
