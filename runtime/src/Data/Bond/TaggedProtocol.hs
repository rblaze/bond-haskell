{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies #-}
module Data.Bond.TaggedProtocol where

import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.TypeDef as TD
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.StructDef

import Data.Bond.Default
import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.Schema (getSchema)
import Data.Bond.Wire

import Control.Applicative hiding (optional)
import Control.Arrow ((&&&), second)
import Control.Monad
import Control.Monad.Reader (ReaderT(..), asks)
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Vector as V

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

type GetContext = SchemaDef
type BinReader = ReaderT GetContext B.Get
type PutContext = (SchemaDef, M.Map Ordinal FD.FieldDef)
type BinWriter = ReaderT PutContext B.PutM

class BondProto t => TaggedProtocol t where
    putFieldHeader :: BondDataType -> Ordinal -> BondPut t
    getFieldHeader :: BondGet t (BondDataType, Ordinal)
    skipStruct :: BondGet t ()
    skipRestOfStruct :: BondGet t ()
    skipType :: TaggedProtocol t => BondDataType -> BondGet t ()

getStruct :: (TaggedProtocol t, ReaderM t ~ BinReader, BondStruct a) => StructLevel -> BondGet t a
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

putStruct :: (TaggedProtocol t, WriterM t ~ BinWriter, BondStruct a) => StructLevel -> a -> BondPut t
putStruct level a = do
    t <- checkPutType bT_STRUCT
    schema <- BondPut (asks fst)
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    plocal (second (const fieldsInfo)) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (TaggedProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondStruct a) => a -> BondPut t
putBaseStruct v = do
    rootT <- checkPutType bT_STRUCT
    schemaStructs <- BondPut (asks $ structs . fst)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    case base_def struct of
        Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
        Just t -> putAs t $ putStruct BaseStruct v

putField :: forall a t. (TaggedProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondSerializable a) => Ordinal -> a -> BondPut t
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

putTag :: WriterM t ~ ReaderT c B.PutM => BondDataType -> BondPut t
putTag = putWord8 . fromIntegral . fromEnum

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch schemaType streamType =
    unless (schemaType == streamType) $
        fail $ "Schema do not match stream: stream/struct type " ++ show streamType ++ ", schema type " ++ show schemaType

checkPutType :: (Monad m, WriterM t ~ ReaderT PutContext m) => BondDataType -> BondPutM t TD.TypeDef
checkPutType expected = do
    t <- BondPut $ asks (root . fst)
    checkSchemaMismatch (TD.id t) expected
    return t

checkGetType :: (Monad m, ReaderM t ~ ReaderT GetContext m) => BondDataType -> BondGet t TD.TypeDef
checkGetType expected = do
    t <- BondGet $ asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: (Monad m, ReaderM t ~ ReaderT GetContext m) => BondDataType -> BondGet t TD.TypeDef
checkElementGetType expected = do
    t <- BondGet $ asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: (Monad m, ReaderM t ~ ReaderT GetContext m) => BondDataType -> BondGet t TD.TypeDef
checkKeyGetType expected = do
    t <- BondGet $ asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkPutContainerType :: (Monad m, WriterM t ~ ReaderT PutContext m) => BondDataType -> BondPutM t TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: (Monad m, WriterM t ~ ReaderT PutContext m) => BondPutM t (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)

getAs :: (Monad m, ReaderM t ~ ReaderT GetContext m) => TD.TypeDef -> BondGet t a -> BondGet t a
getAs typedef = glocal (\s -> s{root = typedef})

putAs :: (Monad m, WriterM t ~ ReaderT PutContext m) => TD.TypeDef -> BondPut t -> BondPut t
putAs typedef = plocal $ \(s, _) -> (s{root = typedef}, error "uninitialized cache")

binaryDecode :: forall a t. (BondStruct a, BondProto t, ReaderM t ~ ReaderT GetContext B.Get) => Proxy t -> L.ByteString -> Either String a
binaryDecode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
        schema = getSchema (Proxy :: Proxy a)
     in case B.runGetOrFail (runReaderT g schema) s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (L.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)
            Right (_, _, a) -> Right a

binaryEncode :: forall a t. (BondStruct a, BondProto t, WriterM t ~ ReaderT PutContext B.PutM) => Proxy t -> a -> Either String L.ByteString
binaryEncode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
        schema = getSchema (Proxy :: Proxy a)
     in Right $ B.runPut (runReaderT g (schema, error "Thou shalt not touch this"))
