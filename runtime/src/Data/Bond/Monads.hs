{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Data.Bond.Monads where

import {-# SOURCE #-}  Data.Bond.Schema.SchemaDef
import {-# SOURCE #-}  Data.Bond.Schema.FieldDef
import {-# SOURCE #-}  Data.Bond.Proto
import Data.Bond.Types

import Control.Applicative
import Control.Monad.Trans
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Control.Monad.Reader as R
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

type GetContext = SchemaDef
type BondGetM = R.ReaderT GetContext B.Get
newtype BondGet t a = BondGet (BondGetM a)
    deriving (Functor, Applicative, Monad)

type PutContext = (SchemaDef, Map Word16 FieldDef)
newtype BondPutM t a = BondPut (R.ReaderT PutContext B.PutM a)
    deriving (Functor, Applicative, Monad)
type BondPut t = BondPutM t ()

glocal :: BondProto t => (GetContext -> GetContext) -> BondGet t a -> BondGet t a
glocal f (BondGet g) = BondGet (R.local f g)

lookAhead :: BondGet t a -> BondGet t a
lookAhead (BondGet g) = do
    env <- BondGet R.ask
    BondGet $ lift $ B.lookAhead (R.runReaderT g env)

skip :: Int -> BondGet t ()
skip = BondGet . lift . B.skip

bytesRead :: BondGet t Int64
bytesRead = BondGet $ lift B.bytesRead

getWord8 :: BondGet t Word8
getWord8 = BondGet $ lift B.getWord8

getWord16le :: BondGet t Word16
getWord16le = BondGet $ lift B.getWord16le

getWord32le :: BondGet t Word32
getWord32le = BondGet $ lift B.getWord32le

getWord64le :: BondGet t Word64
getWord64le = BondGet $ lift B.getWord64le

getByteString :: Int -> BondGet t BS.ByteString
getByteString = BondGet . lift . B.getByteString

getLazyByteString :: Int64 -> BondGet t BL.ByteString
getLazyByteString = BondGet . lift . B.getLazyByteString

plocal :: BondProto t => (PutContext -> PutContext) -> BondPut t -> BondPut t
plocal f (BondPut g) = BondPut (R.local f g)

putWord8 :: Word8 -> BondPut t
putWord8 = BondPut . lift . B.putWord8

putWord16le :: Word16 -> BondPut t
putWord16le = BondPut . lift . B.putWord16le

putWord32le :: Word32 -> BondPut t
putWord32le = BondPut . lift . B.putWord32le

putWord64le :: Word64 -> BondPut t
putWord64le = BondPut . lift . B.putWord64le

putByteString :: BS.ByteString -> BondPut t
putByteString = BondPut . lift . B.putByteString

putLazyByteString :: BL.ByteString -> BondPut t
putLazyByteString = BondPut . lift . B.putLazyByteString
