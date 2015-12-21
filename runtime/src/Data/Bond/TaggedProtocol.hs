{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, ConstraintKinds #-}
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
import Data.Bond.Wire

import Control.Applicative hiding (optional)
import Control.Arrow ((&&&), second)
import Control.Monad
import Control.Monad.Reader (asks)
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Vector ((!))
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.Map as M
import qualified Data.Vector as V

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

class BondProto t => TaggedProtocol t where
    putFieldHeader :: BondDataType -> Ordinal -> BondPut t (WriterM t)
    getFieldHeader :: BondGet t (ReaderM t) (BondDataType, Ordinal)
    skipStruct :: BondGet t (ReaderM t) ()
    skipRestOfStruct :: BondGet t (ReaderM t) ()
    skipType :: TaggedProtocol t => BondDataType -> BondGet t (ReaderM t) ()

getStruct :: (TaggedProtocol t, ReaderM t ~ B.Get, BondStruct a) => StructLevel -> BondGet t B.Get a
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

putStruct :: (TaggedProtocol t, WriterM t ~ B.PutM, BondStruct a) => StructLevel -> a -> BondPut t B.PutM
putStruct level a = do
    t <- checkPutType bT_STRUCT
    schema <- BondPut (asks fst)
    let struct = structs schema ! fromIntegral (TD.struct_def t)
    let fieldsInfo = M.fromList $ V.toList $ V.map (Ordinal . FD.id &&& id) $ fields struct

    plocal (second (const fieldsInfo)) $ bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (TaggedProtocol t, WriterM t ~ B.PutM, BondStruct a) => a -> BondPut t B.PutM
putBaseStruct v = do
    rootT <- checkPutType bT_STRUCT
    schemaStructs <- BondPut (asks $ structs . fst)
    let struct = schemaStructs ! fromIntegral (TD.struct_def rootT)
    case base_def struct of
        Nothing -> fail "Schema do not match structure: attempt to save base of struct w/o base"
        Just t -> putAs t $ putStruct BaseStruct v

putField :: forall a t m. (TaggedProtocol t, ProtoW t m, BondSerializable a) => Ordinal -> a -> BondPut t (WriterM t)
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

putTag :: BondDataType -> BondPut t B.PutM
putTag = putWord8 . fromIntegral . fromEnum

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch schemaType streamType =
    unless (schemaType == streamType) $
        fail $ "Schema do not match stream: stream/struct type " ++ show streamType ++ ", schema type " ++ show schemaType

checkPutType :: ProtoW t m => BondDataType -> BondPutM t (WriterM t) TD.TypeDef
checkPutType expected = do
    t <- BondPut $ asks (root . fst)
    checkSchemaMismatch (TD.id t) expected
    return t

checkGetType :: ProtoR t m => BondDataType -> BondGet t (ReaderM t) TD.TypeDef
checkGetType expected = do
    t <- BondGet $ asks root
    checkSchemaMismatch (TD.id t) expected
    return t

checkElementGetType :: ProtoR t m => BondDataType -> BondGet t (ReaderM t) TD.TypeDef
checkElementGetType expected = do
    t <- BondGet $ asks (TD.element . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkKeyGetType :: ProtoR t m => BondDataType -> BondGet t (ReaderM t) TD.TypeDef
checkKeyGetType expected = do
    t <- BondGet $ asks (TD.key . root)
    checkSchemaMismatch (TD.id <$> t) (Just expected)
    return (fromJust t)

checkPutContainerType :: ProtoW t m => BondDataType -> BondPutM t (WriterM t) TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: ProtoW t m => BondPutM t (WriterM t) (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)

getAs :: ProtoR t m => TD.TypeDef -> BondGet t (ReaderM t) a -> BondGet t (ReaderM t) a
getAs typedef = glocal (\s -> s{root = typedef})

putAs :: ProtoW t m => TD.TypeDef -> BondPut t (WriterM t) -> BondPut t (WriterM t)
putAs typedef = plocal $ \(s, _) -> (s{root = typedef}, error "uninitialized cache")
