{-# Language MultiWayIf, ScopedTypeVariables, FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Data.Bond.JsonProto (
        JsonProto(..)
    ) where

import Data.Bond.Default
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
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as H
import qualified Data.Map as M
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
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream s) = do
        let (sig, rest) = BL.splitAt 4 s
        if sig == protoSig JsonProto
            then case A.eitherDecode rest of
                    Right v -> put v
                    Left msg -> throwError $ "Bonded recode error: " ++ msg
            else undefined -- FIXME recode stream to JSON

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

jsonEncode :: forall a. BondStruct a => a -> BL.ByteString
jsonEncode a =
    let BondPut g = bondPutStruct a :: BondPut JsonProto
     in case runState (runErrorT g) (error "no object") of
            (Left msg, _) -> error $ "putter returned unexpected error " ++ msg
            (Right (), v) -> A.encode v

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
        let parseField s (ordinal, info) = do
                let fname = M.findWithDefault (fieldName info) "JsonName" (fieldAttrs info)
                case HM.lookup fname obj of
                    Nothing -> return s
                    Just v -> local (const v) $ bondStructGetField ordinal s
        foldM parseField baseStruct $ M.toList $ fieldsInfo (Proxy :: Proxy a)

putField :: forall a b . (Serializable a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut JsonProto
putField p n a = do
    let info = M.findWithDefault (error "unknown field ordinal") n (fieldsInfo p)
    let needToSave = not (equalToDefault (fieldDefault info) a) || fieldModifier info /= optional
    when needToSave $ do
        let fname = M.findWithDefault (fieldName info) "JsonName" (fieldAttrs info)
        A.Object obj <- get
        bondPut a
        v <- get
        put $ A.Object $ HM.insert fname v obj

putList :: forall a. Serializable a => [a] -> BondPut JsonProto
putList xs = do
    vs <- forM xs $ \x -> do
        bondPut x
        get
    put $ A.Array $ V.fromList vs

putMap :: forall k v. (Serializable k, Serializable v) => Map k v -> BondPut JsonProto
putMap m = do
    vs <- flip concatMapM (M.toList m) $ \(k, v) -> do
        bondPut k
        key <- get
        bondPut v
        val <- get
        return [key, val]
    put $ A.Array $ V.fromList vs

jsonDecodeWithSchema :: SchemaDef -> BL.ByteString -> Either String Struct
jsonDecodeWithSchema schema bs = do
    checkSchema schema
    v <- A.eitherDecode bs
    runReader (runErrorT rdr) v
    where
    BondGet rdr = readStruct (root schema)
    readStruct :: TD.TypeDef -> BondGet JsonProto Struct
    readStruct td = do
        let idx = fromIntegral $ TD.struct_def td
        let sd = structs schema ! idx
        parent <- case SD.base_def sd of 
            Nothing -> return Nothing
            Just tdescr -> Just <$> readStruct tdescr
        value <- ask
        useObject "struct" value $ \ obj -> do
            fs <- M.fromList . catMaybes . V.toList <$> V.mapM (readField obj) (SD.fields sd)
            return $ Struct parent fs
    readField obj f = do
        let meta = FD.metadata f
        let Utf8 fname = M.findWithDefault (name meta) (Utf8 "JsonName") (attributes meta)
        case HM.lookup (decodeUtf8 fname) obj of
            Nothing -> return Nothing
            Just v -> do
                fieldValue <- local (const v) (readValue $ FD.typedef f)
                return $ Just (Ordinal $ FD.id f, fieldValue)
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
            v <- ask
            return $ BONDED $ BondedStream $ BL.append (protoSig JsonProto) (A.encode v)
        | TD.id td == bT_STRUCT = STRUCT <$> readStruct td
        | TD.id td == bT_LIST = useArrayOrNull "list" $ \v -> do
            let Just et = TD.element td
            LIST (TD.id et) <$> mapM (\x -> local (const x) (readValue et)) (V.toList v)
        | TD.id td == bT_SET = useArray "set" $ \v -> do
            let Just et = TD.element td
            SET (TD.id et) <$> mapM (\x -> local (const x) (readValue et)) (V.toList v)
        | TD.id td == bT_MAP = useArray "map" $ \v -> do
            let Just kt = TD.key td
            let Just vt = TD.element td
            let readPair ss = case ss of
                    [] -> return []
                    (key:val:xs) -> do
                        ke <- local (const key) $ readValue kt
                        ve <- local (const val) $ readValue vt
                        rest <- readPair xs
                        return $ (ke, ve) : rest
                    _ -> throwError "map key without value"
            MAP (TD.id kt) (TD.id vt) <$> readPair (V.toList v)
        | otherwise = error $ "schema validation bug: unknown field type " ++ bondTypeName (TD.id td)

jsonEncodeWithSchema :: SchemaDef -> Struct -> Either String BL.ByteString
jsonEncodeWithSchema schema s = do
    struct <- checkStructSchema schema s
    let BondPut writer = putStruct (root schema) struct
    case runState (runErrorT writer) (error "no object") of
        (Left msg, _) -> Left msg
        (Right (), v) -> Right $ A.encode v
    where
    putStruct :: TD.TypeDef -> Struct -> BondPut JsonProto
    putStruct td struct = do
        put A.emptyObject
        putStructData td struct
    putStructData td struct = do
        let idx = fromIntegral $ TD.struct_def td
        let sd = structs schema ! idx
        case (SD.base_def sd, base struct) of 
            (Nothing, Nothing) -> return ()
            (Just tdescr, Just baseStruct) -> putStructData tdescr baseStruct
            _ -> error "struct validation bug: inheritance chain in schema do not match one in struct"
        V.mapM_ (putStructField $ fields struct) (SD.fields sd)
    putStructField fieldmap f = do
        let meta = FD.metadata f
        let Utf8 fname = M.findWithDefault (name meta) (Utf8 "JsonName") (attributes meta)
        case M.lookup (Ordinal $ FD.id f) fieldmap of
            Nothing -> return ()
            Just value -> do
                A.Object obj <- get
                putValue (FD.typedef f) value
                v <- get
                put $ A.Object $ HM.insert (decodeUtf8 fname) v obj
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
    putValue td (STRUCT v) | TD.bonded_type td = putValue td (BONDED $ BondedObject v)
    putValue td (STRUCT v) = putStruct td v
    putValue td (LIST _ xs) = do
        let Just etd = TD.element td
        vs <- forM xs $ \x -> do
            putValue etd x
            get
        put $ A.Array $ V.fromList vs
    putValue td (SET _ xs) = do
        let Just etd = TD.element td
        vs <- forM xs $ \x -> do
            putValue etd x
            get
        put $ A.Array $ V.fromList vs
    putValue td (MAP _ _ xs) = do
        let Just vtd = TD.element td
        let Just ktd = TD.key td
        vs <- flip concatMapM xs $ \(k, v) -> do
            putValue ktd k
            key <- get
            putValue vtd v
            val <- get
            return [key, val]
        put $ A.Array $ V.fromList vs
    putValue _ (BONDED (BondedStream stream)) = do
        let (sig, rest) = BL.splitAt 4 stream
        if sig == protoSig JsonProto
            then case A.eitherDecode rest of
                    Right v -> put v
                    Left msg -> throwError $ "Bonded recode error: " ++ msg
            else undefined -- FIXME recode stream to JSON
    putValue td (BONDED (BondedObject struct)) = putStruct td struct
