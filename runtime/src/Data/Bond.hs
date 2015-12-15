{-# Language ScopedTypeVariables #-}
module Data.Bond (
    BondProto,
    FastBinaryProto,
    SimpleBinaryProto,
    SimpleBinaryV1Proto,
    bondRead,
    bondWrite,
    getSchema
  ) where

import Data.Bond.FastBinaryProto
import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.Schema
import Data.Bond.SimpleBinaryProto

import Control.Monad.Reader
import Data.Binary.Get
import Data.Binary.Put
import Data.Proxy
import qualified Data.ByteString.Lazy as L

bondRead :: forall a t . (Schemable a, BondStruct a, BondProto t) => Proxy t -> L.ByteString -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)
bondRead _ s = let BondGet g = bondGetStruct :: BondGet t a
                   schema = getSchema (Proxy :: Proxy a)
                in runGetOrFail (runReaderT g schema) s

bondWrite :: forall a t . (Schemable a, BondStruct a, BondProto t) => Proxy t -> a -> L.ByteString
bondWrite _ v = let BondPut g = bondPutStruct v :: BondPut t
                    schema = getSchema (Proxy :: Proxy a)
                 in runPut (runReaderT g (schema, error "Thou shalt not touch this"))
