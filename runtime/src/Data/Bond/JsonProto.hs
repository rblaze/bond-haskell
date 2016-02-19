{-# Language MultiWayIf, ScopedTypeVariables, FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Data.Bond.JsonProto (
        JsonProto(..)
    ) where

import Data.Bond.Default
import Data.Bond.Marshal
import Data.Bond.Proto
import Data.Bond.Struct
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Internal.BondedUtils
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.SchemaOps
import Data.Bond.Internal.SchemaUtils
import Data.Bond.Internal.TypedSchema

import Data.Bond.Schema.ProtocolType

import Control.Applicative hiding (optional)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Text.Encoding
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

data JsonProto = JsonProto

type ReadM = ErrorT String (Reader A.Value)
type WriteM = ErrorT String (State A.Value)

instance BondProto JsonProto where
    bondRead _ = jsonDecode
    bondWrite _ = jsonEncode
    bondReadWithSchema _ = jsonDecodeWithSchema
    bondWriteWithSchema _ = jsonEncodeWithSchema
    protoSig _ = protoHeader sIMPLE_JSON_PROTOCOL 1

instance Protocol JsonProto where
    type ReaderM JsonProto = ReadM
    type WriterM JsonProto = WriteM

    bondGetStruct = parseStruct
    bondGetBaseStruct = parseStruct

    bondGetBool = do
        v <- ask
        case v of
            A.Bool b -> return b
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
                A.Number n -> do
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
                _ -> throwError "map key without value"
        M.fromList <$> readPair (V.toList v)
    bondGetVector = useArray "vector" $ V.mapM (\e -> local (const e) bondGet)
    bondGetNullable = do
        v <- ask
        case v of
            A.Null -> return Nothing
            A.Array a -> if | V.length a == 0 -> return Nothing
                            | V.length a == 1 -> (Just . head) <$> bondGetList
                            | otherwise -> throwError $ "list of length " ++ show (V.length a) ++ " where nullable expected"
            _ -> typeError "nullable" v
    bondGetBonded = do
        v <- ask
        return $ BondedStream $ BL.append (protoSig JsonProto) (A.encode v)

    bondPutStruct v = do
        put A.emptyObject
        bondStructPut v
    bondPutBaseStruct = bondStructPut
    bondPutField = putField
    bondPutDefNothingField _ _ Nothing = return ()
    bondPutDefNothingField p n (Just v) = putField p n v

    bondPutBool = put . A.Bool
    bondPutUInt8 = put . A.Number . fromIntegral
    bondPutUInt16 = put . A.Number . fromIntegral
    bondPutUInt32 = put . A.Number . fromIntegral
    bondPutUInt64 = put . A.Number . fromIntegral
    bondPutInt8 = put . A.Number . fromIntegral
    bondPutInt16 = put . A.Number . fromIntegral
    bondPutInt32 = put . A.Number . fromIntegral
    bondPutInt64 = put . A.Number . fromIntegral
    bondPutFloat = put . A.Number . fromFloatDigits
    bondPutDouble = put . A.Number . fromFloatDigits
    bondPutString (Utf8 s) = put $ A.String $ decodeUtf8 s
    bondPutWString (Utf16 s) = put $ A.String $ decodeUtf16LE s
    bondPutList = putList
    bondPutNullable Nothing = put A.Null
    bondPutNullable (Just v) = bondPutList [v]
    bondPutHashSet = bondPutList . H.toList
    bondPutSet = bondPutList . S.toList
    bondPutMap = putMap
    bondPutVector xs = do
        vs <- V.forM xs $ \x -> do
            bondPut x
            get
        put $ A.Array vs
    bondPutBlob (Blob b) =
        put $ A.Array $ V.generate (BS.length b) $
            \i -> let w = BS.index b i
                      c = fromIntegral w :: Int8
                   in A.Number (fromIntegral c)
    bondPutBonded = putBonded

putBonded :: forall a. BondStruct a => Bonded a -> BondPut JsonProto
putBonded (BondedObject a) = bondPut a
putBonded s = do
    BondedStream stream <- case bondRecode JsonProto s of
        Left msg -> throwError $ "Bonded recode error: " ++ msg
        Right v -> return v
    case A.eitherDecode (BL.drop 4 stream) of
        Left msg -> throwError $ "Bonded recode error: " ++ msg
        Right v -> put v

typeError :: MonadError String m => String -> A.Value -> m a
typeError s v = throwError $ typename ++ " found where " ++ s ++ " expected"
    where
    typename = case v of
            A.Object _ -> "Object"
            A.Array _  -> "Array"
            A.String _ -> "String"
            A.Number _ -> "Number"
            A.Bool _   -> "Boolean"
            A.Null     -> "Null"

jsonDecode :: forall a. BondStruct a => BL.ByteString -> Either String a
jsonDecode s = do
    v <- A.eitherDecode s
    let BondGet g = bondGetStruct :: BondGet JsonProto a

    runReader (runErrorT g) v

jsonEncode :: forall a. BondStruct a => a -> Either String BL.ByteString
jsonEncode a =
    let BondPut g = bondPutStruct a :: BondPut JsonProto
     in case runState (runErrorT g) (error "no object") of
            (Left msg, _) -> Left msg
            (Right (), v) -> Right $ A.encode v

useObject :: String -> A.Value -> (A.Object -> BondGet JsonProto a) -> BondGet JsonProto a
useObject _ (A.Object v) p = p v
useObject s v _ = typeError s v

useArray :: String -> (A.Array -> BondGet JsonProto a) -> BondGet JsonProto a
useArray s p = do
    v <- ask
    case v of
        A.Array a -> p a
        _ -> typeError s v

useArrayOrNull :: String -> (A.Array -> BondGet JsonProto a) -> BondGet JsonProto a
useArrayOrNull s p = do
    v <- ask
    case v of
        A.Null -> p V.empty
        A.Array a -> p a
        _ -> typeError s v

useNumber :: String -> (Scientific -> BondGet JsonProto a) -> BondGet JsonProto a
useNumber s p = do
    v <- ask
    case v of
        A.Number n -> p n
        _ -> typeError s v

useString :: String -> (Text -> a) -> BondGet JsonProto a
useString s p = do
    v <- ask
    case v of
        A.String str -> return (p str)
        _ -> typeError s v

parseStruct :: forall a . BondStruct a => BondGet JsonProto a
parseStruct = do
    value <- ask
    baseStruct <- bondStructGetBase defaultValue

    useObject "struct" value $ \obj -> do
        let parseField s (ordinal, fieldInfo) = do
                let fieldname = M.findWithDefault (fieldName fieldInfo) "JsonName" (fieldAttrs fieldInfo)
                case HM.lookup fieldname obj of
                    Nothing -> return s
                    Just v -> local (const v) $ bondStructGetField ordinal s
        foldM parseField baseStruct $ M.toList $ structFields $ getSchema (Proxy :: Proxy a)

putField :: forall a b . (BondType a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut JsonProto
putField p ordinal a = do
    let fieldInfo = M.findWithDefault (error "internal error: unknown field ordinal") ordinal (structFields $ getSchema p)
    -- FIXME check all required fields written
    let needToSave = not (equalToDefault (fieldType fieldInfo) a) || fieldModifier fieldInfo /= FieldOptional
    when needToSave $ do
        let fieldname = M.findWithDefault (fieldName fieldInfo) "JsonName" (fieldAttrs fieldInfo)
        A.Object obj <- get
        bondPut a
        v <- get
        put $ A.Object $ HM.insert fieldname v obj

putList :: forall a. BondType a => [a] -> BondPut JsonProto
putList xs = do
    vs <- forM xs $ \x -> do
        bondPut x
        get
    put $ A.Array $ V.fromList vs

putMap :: forall k v. (BondType k, BondType v) => Map k v -> BondPut JsonProto
putMap m = do
    vs <- flip concatMapM (M.toList m) $ \(k, v) -> do
        bondPut k
        key <- get
        bondPut v
        val <- get
        return [key, val]
    put $ A.Array $ V.fromList vs

jsonDecodeWithSchema :: StructSchema -> BL.ByteString -> Either String Struct
jsonDecodeWithSchema rootSchema bs = A.eitherDecode bs >>= runReader (runErrorT rdr)
    where
    BondGet rdr = readStruct rootSchema
    readStruct :: StructSchema -> BondGet JsonProto Struct
    readStruct schema = do
        parent <- case structBase schema of 
            Nothing -> return Nothing
            Just baseSchema -> Just <$> readStruct baseSchema
        value <- ask
        useObject "struct" value $ \ obj -> do
            fs <- M.fromList . catMaybes <$> mapM (readField obj) (M.toList $ structFields schema)
            return $ Struct parent fs
    readField obj (fieldId, fieldInfo) = do
        let fieldname = M.findWithDefault (fieldName fieldInfo) "JsonName" (fieldAttrs fieldInfo)
        case HM.lookup fieldname obj of
            Nothing -> return Nothing
            Just v -> do
                fieldValue <- local (const v) $ readValue (fieldToElementType $ fieldType fieldInfo)
                return $ Just (fieldId, fieldValue)

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
        v <- ask
        return $ BONDED $ BondedStream $ BL.append (protoSig JsonProto) (A.encode v)
    readValue (ElementStruct schema) = STRUCT <$> readStruct schema
    readValue (ElementList element) = useArrayOrNull "list" $ \v ->
        LIST (elementToBondDataType element) <$> forM (V.toList v) (\x ->
            local (const x) (readValue element))
    readValue (ElementSet element) = useArrayOrNull "set" $ \v ->
        SET (elementToBondDataType element) <$> forM (V.toList v) (\x ->
            local (const x) (readValue element))
    readValue (ElementMap key value) = useArray "map" $ \v -> do
        let readPair ss = case ss of
                [] -> return []
                (kobj:vobj:xs) -> do
                    k <- local (const kobj) $ readValue key
                    val <- local (const vobj) $ readValue value
                    rest <- readPair xs
                    return $ (k, val) : rest
                _ -> throwError "map key without value"
        MAP (elementToBondDataType key) (elementToBondDataType value) <$> readPair (V.toList v)

jsonEncodeWithSchema :: StructSchema -> Struct -> Either String BL.ByteString
jsonEncodeWithSchema rootSchema s = do
    struct <- checkStructSchema rootSchema s
    let BondPut writer = putStruct rootSchema struct
    case runState (runErrorT writer) (error "no object") of
        (Left msg, _) -> Left msg
        (Right (), v) -> Right $ A.encode v
    where
    putStruct :: StructSchema -> Struct -> BondPut JsonProto
    putStruct schema struct = do
        put A.emptyObject
        putStructData schema struct
    putStructData schema struct = do
        case (structBase schema, base struct) of 
            (Nothing, Nothing) -> return ()
            (Just baseSchema, Just baseStruct) -> putStructData baseSchema baseStruct
            _ -> error "internal error: inheritance chain in schema do not match one in struct"
        mapM_ (putStructField $ structFields schema) $ M.toList $ fields struct
    putStructField schemamap (fieldId, fieldValue) =
        case M.lookup fieldId schemamap of
            Nothing -> return () -- XXX schemaless operations not implemented
            Just fieldInfo -> do
                let fieldname = M.findWithDefault (fieldName fieldInfo) "JsonName" (fieldAttrs fieldInfo)
                A.Object obj <- get
                putValue (fieldToElementType $ fieldType fieldInfo) fieldValue
                v <- get
                put $ A.Object $ HM.insert fieldname v obj

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
        vs <- forM xs $ \x -> do
            putValue element x
            get
        put $ A.Array $ V.fromList vs
    putValue (ElementSet element) (SET _ xs) = do
        vs <- forM xs $ \x -> do
            putValue element x
            get
        put $ A.Array $ V.fromList vs
    putValue (ElementMap key value) (MAP _ _ xs) = do
        vs <- flip concatMapM xs $ \(k, v) -> do
            putValue key k
            kobj <- get
            putValue value v
            vobj <- get
            return [kobj, vobj]
        put $ A.Array $ V.fromList vs
    putValue (ElementBonded schema) (BONDED stream@BondedStream{}) = do
        BondedStream jsonstream <- case bondRecodeStruct JsonProto schema stream of
            Left msg -> throwError $ "Bonded recode error: " ++ msg
            Right v -> return v
        case A.eitherDecode (BL.drop 4 jsonstream) of
            Left msg -> throwError $ "Bonded recode error: " ++ msg
            Right v -> put v
    putValue (ElementBonded schema) (BONDED (BondedObject struct)) = putStruct schema struct
    putValue _ _ = error "internal error: schema type do not match value type"
