{-# Language ScopedTypeVariables, EmptyDataDecls, TypeFamilies #-}
module Data.Bond.SimpleBinaryProto (
        SimpleBinaryProto(..),
        SimpleBinaryV1Proto(..)
    ) where

import Data.Bond.BinaryUtils
import Data.Bond.Cast
import Data.Bond.Proto
import Data.Bond.Types
import Data.Bond.Utils

import Data.Bond.Schema.ProtocolType

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
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

instance BondProto SimpleBinaryProto where
    type ReaderM SimpleBinaryProto = ReaderT () B.Get
    type WriterM SimpleBinaryProto = ReaderT () B.PutM

    bondDecode = decode
    bondDecodeMarshalled = decodeWithHdr sIMPLE_PROTOCOL 2
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
--        sig <- BondGet getWord32be
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream bs

    bondEncode = encode
    bondEncodeMarshalled = encodeWithHdr sIMPLE_PROTOCOL 2
    bondPutStruct = bondStructPut
    bondPutBaseStruct = bondStructPut
    bondPutField _ = bondPut
    bondPutDefNothingField _ Nothing = fail "can't save empty \"default nothing\" field with untagged protocol"
    bondPutDefNothingField _ (Just v) = bondPut v

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

instance BondProto SimpleBinaryV1Proto where
    type ReaderM SimpleBinaryV1Proto = ReaderT () B.Get
    type WriterM SimpleBinaryV1Proto = ReaderT () B.PutM

    bondDecode = decode
    bondDecodeMarshalled = decodeWithHdr sIMPLE_PROTOCOL 1
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
--        sig <- BondGet getWord32be
        bs <- getLazyByteString (fromIntegral size)
        return $ BondedStream bs

    bondEncode = encode
    bondEncodeMarshalled = encodeWithHdr sIMPLE_PROTOCOL 1
    bondPutStruct = bondStructPut
    bondPutBaseStruct = bondStructPut
    bondPutField _ = bondPut
    bondPutDefNothingField _ Nothing = fail "can't save empty \"default nothing\" field with untagged protocol"
    bondPutDefNothingField _ (Just v) = bondPut v

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

decode :: forall a t. (BondStruct a, BondProto t, ReaderM t ~ ReaderT () B.Get) => t -> BL.ByteString -> Either String a
decode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
     in case B.runGetOrFail (runReaderT g ()) s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a

encode :: forall a t. (BondStruct a, BondProto t, WriterM t ~ ReaderT () B.PutM) => t -> a -> Either String BL.ByteString
encode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
     in Right $ B.runPut (runReaderT g ())
