{-# Language ScopedTypeVariables, FlexibleContexts #-}
module Data.Bond.ContextWriter where

import qualified Data.Bond.Schema.FieldDef as FD
import qualified Data.Bond.Schema.TypeDef as TD
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.SchemaDef

import Data.Bond.Proto
import Data.Bond.Types

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as M

data PutContext = PutContext { putSchema :: SchemaDef, putFields :: M.Map Ordinal FD.FieldDef }

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch schemaType streamType =
    unless (schemaType == streamType) $
        fail $ "Schema do not match stream: stream/struct type " ++ show streamType ++ ", schema type " ++ show schemaType

putAs :: MonadReader PutContext (WriterM t) => TD.TypeDef -> BondPut t -> BondPut t
putAs typedef = local $ \ (PutContext s _) -> PutContext s{root = typedef} (error "uninitialized cache")

checkPutType :: MonadReader PutContext (WriterM t) => BondDataType -> BondPutM t TD.TypeDef
checkPutType expected = do
    t <- asks $ root . putSchema
    checkSchemaMismatch (TD.id t) expected
    return t

checkPutContainerType :: MonadReader PutContext (WriterM t) => BondDataType -> BondPutM t TD.TypeDef
checkPutContainerType expected = do
    t <- checkPutType expected
    let elementT = TD.element t
    when (isNothing elementT) $ fail $ "Malformed schema: " ++ show expected ++
        " expected to be container, but has no element type defined"
    return (fromJust elementT)

checkPutMapType :: MonadReader PutContext (WriterM t) => BondPutM t (TD.TypeDef, TD.TypeDef)
checkPutMapType = do
    t <- checkPutType bT_MAP
    let keyT = TD.key t
    let elementT = TD.element t
    when (isNothing keyT || isNothing elementT) $ fail "Malformed schema: map without key or value types"
    return (fromJust keyT, fromJust elementT)
