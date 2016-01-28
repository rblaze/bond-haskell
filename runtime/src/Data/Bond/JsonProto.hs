{-# Language MultiWayIf, ScopedTypeVariables, FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Data.Bond.JsonProto (
        JsonProto(..)
    ) where

import Debug.Trace

import Data.Bond.Default
import Data.Bond.Proto
import Data.Bond.Types
import Data.Bond.Utils

import Data.Bond.Schema.Modifier
import Data.Bond.Schema.ProtocolType

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Text.Encoding
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data JsonProto = JsonProto

type ReadM = ReaderT Value (Except String)
type WriteM = State Value

instance BondProto JsonProto where
    bondRead _ = jsonDecode
    bondWrite _ = jsonEncode
    protoSig _ = protoHeader sIMPLE_JSON_PROTOCOL 1

instance Protocol JsonProto where
    type ReaderM JsonProto = ReadM
    type WriterM JsonProto = WriteM

    bondGetStruct = parseStruct
    bondGetBaseStruct = parseStruct

    bondGetBool = do
        v <- ask
        case v of
            Bool b -> return b
            _ -> typeError "bool" v
    bondGetUInt8 = useNumber "uint8" $ maybe (throwError "value doesn't fit to uint8") return . toBoundedInteger
    bondGetUInt16 = useNumber "uint16" $ maybe (throwError "value doesn't fit to uint16") return . toBoundedInteger
    bondGetUInt32 = useNumber "uint32" $ maybe (throwError "value doesn't fit to uint32") return . toBoundedInteger
    bondGetUInt64 = useNumber "uint64" $ maybe (throwError "value doesn't fit to uint64") return . toBoundedInteger
    bondGetInt8 = useNumber "int8" $ maybe (throwError "value doesn't fit to int8") return . toBoundedInteger
    bondGetInt16 = useNumber "int16" $ maybe (throwError "value doesn't fit to int16") return . toBoundedInteger
    bondGetInt32 = useNumber "int32" $ maybe (throwError "value doesn't fit to int32") return . toBoundedInteger
    bondGetInt64 = useNumber "int64" $ maybe (throwError "value doesn't fit to int64") return . toBoundedInteger
    bondGetFloat = useNumber "float" (return . toRealFloat)
    bondGetDouble = useNumber "double" (return . toRealFloat)
    bondGetString = useString "string" (Utf8 . encodeUtf8)
    bondGetWString = useString "wstring" (Utf16 . encodeUtf16LE)
    bondGetBlob = useArray "blob" $ \v -> do
        let convert x = case x of
                Number n -> do
                    let byte = toBoundedInteger n :: Maybe Int8
                    when (isNothing byte) $ throwError "value doesn't fit to signed byte"
                    return $ fromIntegral (fromJust byte)
                _ -> typeError "signed byte" x
        xs <- V.mapM convert v
        return $ Blob $ BS.pack $ V.toList xs
    bondGetDefNothing = Just <$> bondGet
    bondGetList = useArray "list" $ \v -> mapM (\e -> local (const e) bondGet) (V.toList v)
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = useArray "map" $ \v -> do
        let readPair ss = case ss of
                [] -> return []
                (key:val:xs) -> do
                    ke <- local (const key) bondGet
                    ve <- local (const val) bondGet
                    rest <- readPair xs
                    return $ (ke, ve) : rest
                _ -> fail "map key without value"
        M.fromList <$> readPair (V.toList v)
    bondGetVector = useArray "vector" $ V.mapM (\e -> local (const e) bondGet)
    bondGetNullable = do
        v <- ask
        case v of
            Null -> return Nothing
            Array a -> if | V.length a == 0 -> return Nothing
                          | V.length a == 1 -> (Just . head) <$> bondGetList
                          | otherwise -> fail $ "list of length " ++ show (V.length a) ++ " where nullable expected"
            _ -> typeError "nullable" v
    bondGetBonded = do
        v <- ask
        return $ BondedStream $ BL.append (protoSig JsonProto) (encode v)

    bondPutStruct v = do
        put emptyObject
        bondStructPut v
    bondPutBaseStruct = bondStructPut
    bondPutField = putField
    bondPutDefNothingField _ _ Nothing = return ()
    bondPutDefNothingField p n (Just v) = putField p n v

    bondPutBool = put . Bool
    bondPutUInt8 = put . Number . fromIntegral
    bondPutUInt16 = put . Number . fromIntegral
    bondPutUInt32 = put . Number . fromIntegral
    bondPutUInt64 = put . Number . fromIntegral
    bondPutInt8 = put . Number . fromIntegral
    bondPutInt16 = put . Number . fromIntegral
    bondPutInt32 = put . Number . fromIntegral
    bondPutInt64 = put . Number . fromIntegral
    bondPutFloat = put . Number . fromFloatDigits
    bondPutDouble = put . Number . fromFloatDigits
    bondPutString (Utf8 s) = put $ String $ decodeUtf8 s
    bondPutWString (Utf16 s) = put $ String $ decodeUtf16LE s
    bondPutList = putList
    bondPutNullable Nothing = put Null
    bondPutNullable (Just v) = bondPutList [v]
    bondPutHashSet = bondPutList . H.toList
    bondPutSet = bondPutList . S.toList
    bondPutMap = putMap
    bondPutVector xs = do
        vs <- V.forM xs $ \x -> do
            bondPut x
            get
        put $ Array vs
    bondPutBlob (Blob b) =
        put $ Array $ V.generate (BS.length b) $
            \i -> let w = BS.index b i
                      c = fromIntegral w :: Int8
                   in Number (fromIntegral c)
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream s) = do
        let (sig, rest) = BL.splitAt 4 s
        if sig == protoSig JsonProto
            then case eitherDecode rest of
                    Right v -> put v
                    Left msg -> fail $ "Bonded recode error: " ++ msg
            else undefined -- FIXME recode stream to JSON

typeError :: MonadError String m => String -> Value -> m a
typeError s v = throwError $ typename ++ " found where " ++ s ++ " expected"
    where
    typename = case v of
            Object _ -> "Object"
            Array _  -> "Array"
            String _ -> "String"
            Number _ -> "Number"
            Bool _   -> "Boolean"
            Null     -> "Null"

jsonDecode :: forall a. BondStruct a => BL.ByteString -> Either String a
jsonDecode s = do
    v <- eitherDecode s
    let BondGet g = bondGetStruct :: BondGet JsonProto a

    runExcept (runReaderT g v)

jsonEncode :: forall a. BondStruct a => a -> BL.ByteString
jsonEncode a =
    let BondPut g = bondPutStruct a :: BondPut JsonProto
        v = execState g (error "no object")
     in encode v

useObject :: String -> Value -> (Object -> BondGet JsonProto a) -> BondGet JsonProto a
useObject _ (Object v) p = p v
useObject s v _ = typeError s v

useArray :: String -> (Array -> BondGet JsonProto a) -> BondGet JsonProto a
useArray s p = do
    v <- ask
    case v of
        Array a -> p a
        _ -> typeError s v

useNumber :: String -> (Scientific -> BondGet JsonProto a) -> BondGet JsonProto a
useNumber s p = do
    v <- ask
    case v of
        Number n -> p n
        _ -> typeError s v

useString :: String -> (Text -> a) -> BondGet JsonProto a
useString s p = do
    v <- ask
    case v of
        String str -> return (p str)
        _ -> typeError s v

parseStruct :: forall a . BondStruct a => BondGet JsonProto a
parseStruct = do
    value <- ask
    base <- bondStructGetBase defaultValue

    useObject "struct" value $ \obj -> do
        let parseField s (ordinal, info) = do
                let fname = M.findWithDefault (fieldName info) "JsonName" (fieldAttrs info)
                case HM.lookup fname obj of
                    Nothing -> return s
                    Just v -> local (const v) $ bondStructGetField ordinal s
        foldM parseField base $ M.toList $ fieldsInfo (Proxy :: Proxy a)

putField :: forall a b . (Serializable a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut JsonProto
putField p n a = do
    let info = M.findWithDefault (error "unknown field ordinal") n (fieldsInfo p)
    let needToSave = not (equalToDefault (fieldDefault info) a) || fieldModifier info /= optional
    when needToSave $ do
        let fname = M.findWithDefault (fieldName info) "JsonName" (fieldAttrs info)
        Object obj <- get
        bondPut a
        v <- get
        put $ Object $ HM.insert fname v obj

putList :: forall a. Serializable a => [a] -> BondPut JsonProto
putList xs = do
    vs <- forM xs $ \x -> do
        bondPut x
        get
    put $ Array $ V.fromList vs

putMap :: forall k v. (Serializable k, Serializable v) => Map k v -> BondPut JsonProto
putMap m = do
    vs <- flip concatMapM (M.toList m) $ \(k, v) -> do
        bondPut k
        key <- get
        bondPut v
        val <- get
        return [key, val]
    put $ Array $ V.fromList vs
