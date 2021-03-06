{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Data.Bond.Internal.TaggedProtocol where

import Data.Bond.Schema.BondDataType

import Data.Bond.Struct
import Data.Bond.TypedSchema
import Data.Bond.Types
import Data.Bond.Internal.BinaryUtils
import Data.Bond.Internal.Default
import Data.Bond.Internal.OrdinalSet
import Data.Bond.Internal.Protocol
import Data.Bond.Internal.SchemaOps
import Data.Bond.Internal.SchemaUtils

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.Proxy
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Map.Strict as MS

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

class Protocol t => TaggedProtocol t where
    getFieldHeader :: BondGet t (BondDataType, Ordinal)
    getListHeader :: BondGet t (BondDataType, Int)
    getTaggedStruct :: BondGet t Struct
    putFieldHeader :: BondDataType -> Ordinal -> BondPut t
    putListHeader :: (Integral a, FiniteBits a) => BondDataType -> a -> BondPut t
    putTaggedStruct :: Struct -> BondPut t
    skipStruct :: BondGet t ()
    skipRestOfStruct :: BondGet t ()
    skipType :: BondDataType -> BondGet t ()

getStruct :: forall a t. (Functor (ReaderM t), Monad (ReaderM t), TaggedProtocol t, BondStruct a) => StructLevel -> BondGet t a
getStruct level = do
    let schema = getSchema (Proxy :: Proxy a)
    let fieldsMap = structFields schema
    b <- bondStructGetBase defaultValue
    -- iterate over stream, update fields
    let readField wiretype ordinal s =
            if M.member ordinal fieldsMap
                then bondStructGetField ordinal s
                else do
                    skipType wiretype -- unknown field, ignore it
                    return s
    let loop (s, ords) = do
            (wiretype, ordinal) <- getFieldHeader
            if | wiretype == bT_STOP && level == BaseStruct -> fail "BT_STOP found where BT_STOP_BASE expected"
               | wiretype == bT_STOP && level == TopLevelStruct -> return (s, ords)
               | wiretype == bT_STOP_BASE && level == BaseStruct -> return (s, ords)
               | wiretype == bT_STOP_BASE && level == TopLevelStruct -> skipRestOfStruct >> return (s, ords)
               | otherwise -> do
                    s' <- readField wiretype ordinal s
                    loop (s', deleteOrdinal ordinal ords)
    (value, notRead) <- loop (b, structRequiredOrdinals schema)
    unless (isEmptySet notRead) $ fail $ "required fields not read: " ++ show (map (getFieldName schema) $ toOrdinalList notRead)
    return value

putStruct :: (WriterM t ~ ErrorT String B.PutM, TaggedProtocol t, BondStruct a) => StructLevel -> a -> BondPut t
putStruct level a = do
    bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (WriterM t ~ ErrorT String B.PutM, TaggedProtocol t, BondStruct a) => a -> BondPut t
putBaseStruct = putStruct BaseStruct

putField :: forall a b t. (Monad (BondPutM t), TaggedProtocol t, BondType a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut t
putField p ordinal value = do
    let tag = getWireType (Proxy :: Proxy a)
    let info = M.findWithDefault (error "internal error: unknown field ordinal") ordinal (structFields $ getSchema p)
    let needToSave = not (equalToDefault (fieldType info) value) || fieldModifier info /= FieldOptional
    when needToSave $ do
        putFieldHeader tag ordinal
        bondPut value

putTag :: WriterM t ~ ErrorT String B.PutM => BondDataType -> BondPut t
putTag = putWord8 . fromIntegral . fromEnum

binaryDecode :: forall a t. (ReaderM t ~ B.Get, BondStruct a, Protocol t) => t -> BL.ByteString -> Either String a
binaryDecode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
     in case B.runGetOrFail g s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a

binaryEncode :: forall a t. (WriterM t ~ ErrorT String B.PutM, BondStruct a, Protocol t) => t -> a -> Either String BL.ByteString
binaryEncode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
     in tryPut g

getTaggedData :: forall t. (ReaderM t ~ B.Get, TaggedProtocol t) => BondGet t Struct
getTaggedData = fieldLoop $ Struct Nothing M.empty
    where
    getValue :: BondDataType -> BondGet t Value
    getValue t =
        if | t == bT_BOOL -> BOOL <$> bondGetBool
           | t == bT_UINT8 -> UINT8 <$> bondGetUInt8
           | t == bT_UINT16 -> UINT16 <$> bondGetUInt16
           | t == bT_UINT32 -> UINT32 <$> bondGetUInt32
           | t == bT_UINT64 -> UINT64 <$> bondGetUInt64
           | t == bT_FLOAT -> FLOAT <$> bondGetFloat
           | t == bT_DOUBLE -> DOUBLE <$> bondGetDouble
           | t == bT_STRING -> STRING <$> bondGetString
           | t == bT_STRUCT -> STRUCT <$> getTaggedStruct
           | t == bT_LIST -> do
                (td, n) <- getListHeader
                LIST td <$> replicateM n (getValue td)
           | t == bT_SET -> do
                (td, n) <- getListHeader
                SET td <$> replicateM n (getValue td)
           | t == bT_MAP -> do
                tk <- BondDataType . fromIntegral <$> getWord8
                tv <- BondDataType . fromIntegral <$> getWord8
                n <- getVarInt
                MAP tk tv <$> replicateM n (do
                    k <- getValue tk
                    v <- getValue tv
                    return (k, v))
           | t == bT_INT8 -> INT8 <$> bondGetInt8
           | t == bT_INT16 -> INT16 <$> bondGetInt16
           | t == bT_INT32 -> INT32 <$> bondGetInt32
           | t == bT_INT64 -> INT64 <$> bondGetInt64
           | t == bT_WSTRING -> WSTRING <$> bondGetWString
           | otherwise -> fail $ "invalid field type " ++ bondTypeName t
    setField s o v = return $ s { fields = MS.insert o v (fields s) }
    fieldLoop s = do
        (t, o) <- getFieldHeader
        if | t == bT_STOP -> return s
           | t == bT_STOP_BASE -> fieldLoop $ Struct (Just s) M.empty
           | otherwise -> getValue t >>= setField s o >>= fieldLoop

readTagged :: forall t. (ReaderM t ~ B.Get, TaggedProtocol t) => t -> BL.ByteString -> Either String Struct
readTagged _ s =
    let BondGet g = getTaggedStruct :: BondGet t Struct
     in case B.runGetOrFail g s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> Right a

readTaggedWithSchema :: forall t. (ReaderM t ~ B.Get, TaggedProtocol t) => t -> StructSchema -> BL.ByteString -> Either String Struct
readTaggedWithSchema _ schema s =
    let BondGet g = getTaggedStruct :: BondGet t Struct
     in case B.runGetOrFail g s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (BL.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (BL.length rest)
            Right (_, _, a) -> checkStructSchema schema a

putTaggedData :: forall t. (MonadError String (BondPutM t), WriterM t ~ ErrorT String B.PutM, TaggedProtocol t) => Struct -> BondPut t
putTaggedData s = do
    case base s of
        Just b -> putTaggedData b >> putTag bT_STOP_BASE
        Nothing -> return ()
    forM_ (M.toList $ fields s) $ \ (o, v) -> do
        let (typ, writer) = saveValue v
        putFieldHeader typ o
        writer
    where
    saveValue :: Value -> (BondDataType, BondPut t)
    saveValue (BOOL v) = (bT_BOOL, bondPutBool v)
    saveValue (INT8 v) = (bT_INT8, bondPutInt8 v)
    saveValue (INT16 v) = (bT_INT16, bondPutInt16 v)
    saveValue (INT32 v) = (bT_INT32, bondPutInt32 v)
    saveValue (INT64 v) = (bT_INT64, bondPutInt64 v)
    saveValue (UINT8 v) = (bT_UINT8, bondPutUInt8 v)
    saveValue (UINT16 v) = (bT_UINT16, bondPutUInt16 v)
    saveValue (UINT32 v) = (bT_UINT32, bondPutUInt32 v)
    saveValue (UINT64 v) = (bT_UINT64, bondPutUInt64 v)
    saveValue (FLOAT v) = (bT_FLOAT, bondPutFloat v)
    saveValue (DOUBLE v) = (bT_DOUBLE, bondPutDouble v)
    saveValue (STRING v) = (bT_STRING, bondPutString v)
    saveValue (WSTRING v) = (bT_WSTRING, bondPutWString v)
    saveValue (STRUCT v) = (bT_STRUCT, putTaggedStruct v)
    saveValue (LIST td xs) = (bT_LIST, putListHeader td (length xs) >> mapM_ (saveTypedValue td) xs)
    saveValue (SET td xs) = (bT_SET, putListHeader td (length xs) >> mapM_ (saveTypedValue td) xs)
    saveValue (MAP tk tv xs) = (bT_MAP, do
        putTag tk
        putTag tv
        putVarInt $ length xs
        forM_ xs $ \ (k, v) -> do
            saveTypedValue tk k
            saveTypedValue tv v
      )
    saveValue (BONDED (BondedObject v)) = (bT_STRUCT, putTaggedStruct v)
    saveValue (BONDED _) = (bT_STRUCT, throwError "not implemented: should decode bonded values before tagged writes")
            -- FIXME be smart here
            -- same sig - copy stream
            -- tagged sig - unmarshal struct blindly, then marshal
            -- untagged sig - return error, but do untagged decoding while matching struct with schema
    saveTypedValue td v
        = let (realtd, writer) = saveValue v
           in if td == realtd
                then writer
                else throwError $ "element type do not match container type: " ++ bondTypeName td ++ " expected, " ++ bondTypeName realtd ++ " found"

writeTagged :: forall t. (WriterM t ~ ErrorT String B.PutM, TaggedProtocol t) => t -> Struct -> Either String BL.ByteString
writeTagged _ a = let BondPut g = putTaggedStruct a :: BondPut t
                   in tryPut g

writeTaggedWithSchema :: (WriterM t ~ ErrorT String B.PutM, TaggedProtocol t) => t -> StructSchema -> Struct -> Either String BL.ByteString
writeTaggedWithSchema t schema struct = checkStructSchema schema struct >>= writeTagged t
