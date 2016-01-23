{-# Language MultiWayIf, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module Data.Bond.JsonProto (
        JsonProto(..)
    ) where

import Data.Bond.ContextWriter
import Data.Bond.Default
import Data.Bond.Proto
import Data.Bond.Schema (getSchema)
import Data.Bond.Types
import Data.Bond.Utils
import Data.Bond.Wire

import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.TypeDef as TD
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.ProtocolType
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.StructDef

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data JsonProto = JsonProto

data ReaderCtx = ReaderCtx { rdSchema :: SchemaDef, rdValue :: Value }
type ReadM = ReaderT ReaderCtx Parser

type WriteM = RWS PutContext () Value

instance BondProto JsonProto where
    bondRead _ = jsonDecode
    bondWrite _ = jsonEncode

instance Protocol JsonProto where
    type ReaderM JsonProto = ReadM
    type WriterM JsonProto = WriteM

    bondGetStruct = parseStruct
    bondGetBaseStruct = parseStruct

    bondGetBool = do
        v <- asks rdValue
        case v of
            Bool b -> return b
            _ -> BondGet $ lift $ typeMismatch "bool" v
    bondGetUInt8 = useNumber "uint8" $ maybe (fail "value doesn't fit to uint8") return . toBoundedInteger
    bondGetUInt16 = useNumber "uint16" $ maybe (fail "value doesn't fit to uint16") return . toBoundedInteger
    bondGetUInt32 = useNumber "uint32" $ maybe (fail "value doesn't fit to uint32") return . toBoundedInteger
    bondGetUInt64 = useNumber "uint64" $ maybe (fail "value doesn't fit to uint64") return . toBoundedInteger
    bondGetInt8 = useNumber "int8" $ maybe (fail "value doesn't fit to int8") return . toBoundedInteger
    bondGetInt16 = useNumber "int16" $ maybe (fail "value doesn't fit to int16") return . toBoundedInteger
    bondGetInt32 = useNumber "int32" $ maybe (fail "value doesn't fit to int32") return . toBoundedInteger
    bondGetInt64 = useNumber "int64" $ maybe (fail "value doesn't fit to int64") return . toBoundedInteger
    bondGetFloat = useNumber "float" (return . toRealFloat)
    bondGetDouble = useNumber "double" (return . toRealFloat)
    bondGetString = useString "string" (Utf8 . encodeUtf8)
    bondGetWString = useString "wstring" (Utf16 . encodeUtf16LE)
    bondGetBlob = useArray "blob" $ \v -> do
        let convert x = case x of
                Number n -> do
                    let byte = toBoundedInteger n :: Maybe Int8
                    when (isNothing byte) $ fail "value doesn't fit to signed byte"
                    return $ fromIntegral (fromJust byte)
                _ -> BondGet $ lift $ typeMismatch "signed byte" x
        xs <- V.mapM convert v
        return $ Blob $ BS.pack $ V.toList xs
    bondGetDefNothing = Just <$> bondGet
    bondGetList = useArray "list" $ \v -> do
        t <- asks (TD.element . root . rdSchema)
        when (isNothing t) $ fail "no element type defined in list"
        mapM (\e -> getAsTypeValue (fromJust t) e bondGet) (V.toList v)
    bondGetHashSet = H.fromList <$> bondGetList
    bondGetSet = S.fromList <$> bondGetList
    bondGetMap = useArray "map" $ \v -> do
        tk <- asks (TD.key . root . rdSchema)
        tv <- asks (TD.element . root . rdSchema)
        when (isNothing tk) $ fail "no key type defined in map"
        when (isNothing tv) $ fail "no element type defined in map"
        let readPair ss = case ss of
                [] -> return []
                (key:val:xs) -> do
                    ke <- getAsTypeValue (fromJust tk) key bondGet
                    ve <- getAsTypeValue (fromJust tv) val bondGet
                    rest <- readPair xs
                    return $ (ke, ve) : rest
                _ -> fail "map key without value"
        M.fromList <$> readPair (V.toList v)
    bondGetVector = useArray "vector" $ \v -> do
        t <- asks (TD.element . root . rdSchema)
        when (isNothing t) $ fail "no element type defined in list"
        V.mapM (\e -> getAsTypeValue (fromJust t) e bondGet) v
    bondGetNullable = do
        v <- asks rdValue
        case v of
            Null -> return Nothing
            Array a -> if | V.length a == 0 -> return Nothing
                          | V.length a == 1 -> (Just . head) <$> bondGetList
                          | otherwise -> fail $ "list of length " ++ show (V.length a) ++ " where nullable expected"
            _ -> BondGet $ lift $ typeMismatch "nullable" v
    bondGetBonded = do
        v <- asks rdValue
        return $ BondedStream $ encode v

    bondPutStruct v = do
        put emptyObject
        putStruct v
    bondPutBaseStruct = putBaseStruct
    bondPutField = putField
    bondPutDefNothingField _ Nothing = return ()
    bondPutDefNothingField n (Just v) = putField n v

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
        t <- checkPutContainerType bT_LIST

        vs <- V.forM xs $ \x -> do
            putAs t $ bondPut x
            get
        put $ Array vs
    bondPutBlob (Blob b) =
        put $ Array $ V.generate (BS.length b) $
            \i -> let w = BS.index b i
                      c = fromIntegral w :: Int8
                   in Number (fromIntegral c)
    bondPutBonded (BondedObject _) = undefined
    bondPutBonded (BondedStream s) =
        case eitherDecode s of
            Right v -> put v
            Left msg -> fail msg

jsonDecode :: forall a. BondStruct a => BL.ByteString -> Either String a
jsonDecode s = do
    v <- eitherDecode s
    let BondGet g = bondGetStruct :: BondGet JsonProto a
    let schema = getSchema (Proxy :: Proxy a)
    let parser = runReaderT g

    parseEither parser (ReaderCtx schema v)

jsonEncode :: forall a. BondStruct a => a -> BL.ByteString
jsonEncode a =
    let BondPut g = bondPutStruct a :: BondPut JsonProto
        schema = getSchema (Proxy :: Proxy a)
        (v, _) = execRWS g (PutContext schema (error "empty cache")) (error "no object")
     in encode v

useObject :: String -> Value -> (Object -> BondGet JsonProto a) -> BondGet JsonProto a
useObject _ (Object v) p = p v
useObject s v _ = BondGet $ lift $ typeMismatch s v

useArray :: String -> (Array -> BondGet JsonProto a) -> BondGet JsonProto a
useArray s p = do
    v <- asks rdValue
    case v of
        Array a -> p a
        _ -> BondGet $ lift $ typeMismatch s v

useNumber :: String -> (Scientific -> BondGet JsonProto a) -> BondGet JsonProto a
useNumber s p = do
    v <- asks rdValue
    case v of
        Number n -> p n
        _ -> BondGet $ lift $ typeMismatch s v

useString :: String -> (Text -> a) -> BondGet JsonProto a
useString s p = do
    v <- asks rdValue
    case v of
        String str -> return (p str)
        _ -> BondGet $ lift $ typeMismatch s v

getAsTypeValue :: TD.TypeDef -> Value -> BondGet JsonProto a -> BondGet JsonProto a
getAsTypeValue td v = local $ \ (ReaderCtx s _) -> ReaderCtx s{root = td} v

checkGetTypeValue :: BondDataType -> BondGet JsonProto TD.TypeDef
checkGetTypeValue expected = do
    t <- asks (root . rdSchema)
    checkSchemaMismatch (TD.id t) expected
    return t

parseStruct :: forall a. BondStruct a => BondGet JsonProto a
parseStruct = do
    rootT <- checkGetTypeValue bT_STRUCT
    schemaStructs <- asks (structs . rdSchema)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    value <- asks rdValue
    base <- case base_def struct of
        Nothing -> return defaultValue
        Just t -> getAsTypeValue t value (bondStructGetBase defaultValue)

    useObject "struct" value $ \obj -> do
        let parseField s f = do
                let Utf8 fieldName = M.findWithDefault (name $ FD.metadata f) (fromString "JsonName") (attributes $ FD.metadata f)
                let ordinal = Ordinal (FD.id f)
                case HM.lookup (decodeUtf8 fieldName) obj of
                    Nothing -> return s
                    Just v -> getAsTypeValue (FD.typedef f) v $ bondStructGetField ordinal s
        foldlM parseField base (fields struct)

putField :: forall a . Serializable a => Ordinal -> a -> BondPut JsonProto
putField n a = do
    fieldTypes <- asks putFields
    let Just f = M.lookup n fieldTypes
    let t = FD.typedef f
    let tag = getWireType (Proxy :: Proxy a)
    checkSchemaMismatch (TD.id t) tag

    let needToSave = not (equalToDefault (default_value $ FD.metadata f) a) ||
           modifier (FD.metadata f) /= optional
    when needToSave $ do
        let Utf8 fieldName = M.findWithDefault (name $ FD.metadata f) (fromString "JsonName") (attributes $ FD.metadata f)
        Object obj <- get
        putAs t $ bondPut a
        v <- get
        put $ Object $ HM.insert (decodeUtf8 fieldName) v obj

putStruct :: BondStruct a => a -> BondPut JsonProto
putStruct v = do
    t <- checkPutType bT_STRUCT
    schema <- asks putSchema
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct
    local (\s -> s { putFields = fieldsInfo }) $ bondStructPut v

putBaseStruct :: BondStruct a => a -> BondPut JsonProto
putBaseStruct v = do
    rootT <- checkPutType bT_STRUCT
    schemaStructs <- asks $ structs . putSchema
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    case base_def struct of
        Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
        Just t -> putAs t $ putStruct v

putList :: forall a. Serializable a => [a] -> BondPut JsonProto
putList xs = do
    t <- checkPutContainerType bT_LIST

    vs <- forM xs $ \x -> do
        putAs t $ bondPut x
        get
    put $ Array $ V.fromList vs

putMap :: forall k v. (Serializable k, Serializable v) => Map k v -> BondPut JsonProto
putMap m = do
    (tk, tv) <- checkPutMapType

    vs <- flip concatMapM (M.toList m) $ \(k, v) -> do
        putAs tk $ bondPut k
        key <- get
        putAs tv $ bondPut v
        val <- get
        return [key, val]
    put $ Array $ V.fromList vs
