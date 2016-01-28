{-# Language ScopedTypeVariables, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Data.Bond.TaggedProtocol where

import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.Modifier

import Data.Bond.BinaryUtils
import Data.Bond.Default
import Data.Bond.Proto
import Data.Bond.Types
import Data.Bond.Wire

import Control.Monad
import Data.Proxy
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

data StructLevel = TopLevelStruct | BaseStruct
    deriving (Show, Eq)

class Protocol t => TaggedProtocol t where
    putFieldHeader :: BondDataType -> Ordinal -> BondPut t
    getFieldHeader :: BondGet t (BondDataType, Ordinal)
    skipStruct :: BondGet t ()
    skipRestOfStruct :: BondGet t ()
    skipType :: TaggedProtocol t => BondDataType -> BondGet t ()

getStruct :: forall a t. (Functor (ReaderM t), Monad (ReaderM t), TaggedProtocol t, BondStruct a) => StructLevel -> BondGet t a
getStruct level = do
    let fields = fieldsInfo (Proxy :: Proxy a)
    base <- bondStructGetBase defaultValue
    -- iterate over stream, update fields
    let readField wiretype ordinal s =
            if M.member ordinal fields
                then bondStructGetField ordinal s
                else do
                    skipType wiretype -- unknown field, ignore it
                    return s
    let loop s = do
            (wiretype, ordinal) <- getFieldHeader
            if | wiretype == bT_STOP && level == BaseStruct -> fail "BT_STOP found where BT_STOP_BASE expected"
               | wiretype == bT_STOP && level == TopLevelStruct -> return s
               | wiretype == bT_STOP_BASE && level == BaseStruct -> return s
               | wiretype == bT_STOP_BASE && level == TopLevelStruct -> skipRestOfStruct >> return s
               | otherwise -> readField wiretype ordinal s >>= loop

    loop base

putStruct :: (WriterM t ~ B.PutM, TaggedProtocol t, BondStruct a) => StructLevel -> a -> BondPut t
putStruct level a = do
    bondStructPut a
    case level of
        TopLevelStruct -> putTag bT_STOP
        BaseStruct -> putTag bT_STOP_BASE

putBaseStruct :: (WriterM t ~ B.PutM, TaggedProtocol t, BondStruct a) => a -> BondPut t
putBaseStruct = putStruct BaseStruct

putField :: forall a b t. (Monad (WriterM t), TaggedProtocol t, Serializable a, BondStruct b) => Proxy b -> Ordinal -> a -> BondPut t
putField p n a = do
    let tag = getWireType (Proxy :: Proxy a)
    let info = M.findWithDefault (error "unknown field ordinal") n (fieldsInfo p)
    let needToSave = not (equalToDefault (fieldDefault info) a) || fieldModifier info /= optional
    when needToSave $ do
        putFieldHeader tag n
        bondPut a

putTag :: WriterM t ~ B.PutM => BondDataType -> BondPut t
putTag = putWord8 . fromIntegral . fromEnum

binaryDecode :: forall a t. (ReaderM t ~ B.Get, BondStruct a, Protocol t) => t -> L.ByteString -> Either String a
binaryDecode _ s =
    let BondGet g = bondGetStruct :: BondGet t a
     in case B.runGetOrFail g s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (L.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)
            Right (_, _, a) -> Right a

binaryEncode :: forall a t. (WriterM t ~ B.PutM, BondStruct a, Protocol t) => t -> a -> L.ByteString
binaryEncode _ a =
    let BondPut g = bondPutStruct a :: BondPut t
     in B.runPut g
