{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Data.Bond.TaggedProtocol (
    module Data.Bond.TaggedProtocol,
    module Data.Bond.ContextWriter
  ) where

import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.TypeDef as TD
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Metadata
import Data.Bond.Schema.Modifier
import Data.Bond.Schema.SchemaDef
import Data.Bond.Schema.StructDef

import Data.Bond.BinaryUtils
import Data.Bond.Default
import Data.Bond.Proto
import Data.Bond.Schema (getSchema)
import Data.Bond.Types
import Data.Bond.ContextWriter
import Data.Bond.Wire

import Control.Applicative hiding (optional)
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Reader
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
    schemaStructs <- asks structs
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
    schema <- asks putSchema
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    local (\ s -> s { putFields = fieldsInfo }) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (TaggedProtocol t, WriterM t ~ ReaderT PutContext B.PutM, BondStruct a) => a -> BondPut t
putBaseStruct v = do
    rootT <- checkPutType bT_STRUCT
    schemaStructs <- asks $ structs . putSchema
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    case base_def struct of
        Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
        Just t -> putAs t $ putStruct BaseStruct v

putField :: forall a t. (TaggedProtocol t, WriterM t ~ ReaderT PutContext B.PutM, Serializable a) => Ordinal -> a -> BondPut t
putField n a = do
    fieldTypes <- asks putFields
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

checkGetType :: MonadReader GetContext (ReaderM t) => BondDataType -> BondGet t TD.TypeDef
checkGetType expected = do
    t <- asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: MonadReader GetContext (ReaderM t) => BondDataType -> BondGet t TD.TypeDef
checkElementGetType expected = do
    t <- asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: MonadReader GetContext (ReaderM t) => BondDataType -> BondGet t TD.TypeDef
checkKeyGetType expected = do
    t <- asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

getAs :: MonadReader GetContext (ReaderM t) => TD.TypeDef -> BondGet t a -> BondGet t a
getAs typedef = local (\s -> s{root = typedef})

binaryDecode :: forall a t. (BondStruct a, BondProto t, ReaderM t ~ ReaderT GetContext B.Get) => t -> L.ByteString -> Either String a
binaryDecode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
        schema = getSchema (Proxy :: Proxy a)
     in case B.runGetOrFail (runReaderT g schema) s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (L.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)
            Right (_, _, a) -> Right a

binaryEncode :: forall a t. (BondStruct a, BondProto t, WriterM t ~ ReaderT PutContext B.PutM) => t -> a -> Either String L.ByteString
binaryEncode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
        schema = getSchema (Proxy :: Proxy a)
     in Right $ B.runPut (runReaderT g (PutContext schema (error "Thou shalt not touch this")))
