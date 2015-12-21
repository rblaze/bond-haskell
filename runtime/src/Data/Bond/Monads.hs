{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Data.Bond.Monads where

import {-# SOURCE #-}  Data.Bond.Schema.SchemaDef
import {-# SOURCE #-}  Data.Bond.Schema.FieldDef

import Data.Bond.Types
import Data.Bond.Wire

import Control.Applicative
import Control.Monad.Trans
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Control.Monad.Reader as R
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

type GetContext = SchemaDef
type BondGetM = R.ReaderT GetContext
newtype BondGet t m a = BondGet (BondGetM m a)
    deriving (Functor, Applicative, Monad)

type PutContext = (SchemaDef, Map Ordinal FieldDef)
newtype BondPutM t m a = BondPut (R.ReaderT PutContext m a)
    deriving (Functor, Applicative, Monad)
type BondPut t m = BondPutM t m ()

-- XXX until I can upgrade to binary >= 0.7.2.0 (and drop lts-2 support), isolate is unavailable
isolate :: Int -> BondGet t B.Get a -> BondGet t B.Get a
isolate _ = Prelude.id
--isolate n (BondGet g) = do
--    env <- BondGet R.ask
--    BondGet $ lift $ B.isolate n (R.runReaderT g env)

lookAhead :: BondGet t B.Get a -> BondGet t B.Get a
lookAhead (BondGet g) = do
    env <- BondGet R.ask
    BondGet $ lift $ B.lookAhead (R.runReaderT g env)

skip :: Int -> BondGet t B.Get ()
skip = BondGet . lift . B.skip

bytesRead :: BondGet t B.Get Int64
bytesRead = BondGet $ lift B.bytesRead

getWord8 :: BondGet t B.Get Word8
getWord8 = BondGet $ lift B.getWord8

getWord16le :: BondGet t B.Get Word16
getWord16le = BondGet $ lift B.getWord16le

getWord32le :: BondGet t B.Get Word32
getWord32le = BondGet $ lift B.getWord32le

getWord64le :: BondGet t B.Get Word64
getWord64le = BondGet $ lift B.getWord64le

getByteString :: Int -> BondGet t B.Get BS.ByteString
getByteString = BondGet . lift . B.getByteString

getLazyByteString :: Int64 -> BondGet t B.Get BL.ByteString
getLazyByteString = BondGet . lift . B.getLazyByteString

putWord8 :: Word8 -> BondPut t B.PutM
putWord8 = BondPut . lift . B.putWord8

putWord16le :: Word16 -> BondPut t B.PutM
putWord16le = BondPut . lift . B.putWord16le

putWord32le :: Word32 -> BondPut t B.PutM
putWord32le = BondPut . lift . B.putWord32le

putWord64le :: Word64 -> BondPut t B.PutM
putWord64le = BondPut . lift . B.putWord64le

putByteString :: BS.ByteString -> BondPut t B.PutM
putByteString = BondPut . lift . B.putByteString

putLazyByteString :: BL.ByteString -> BondPut t B.PutM
putLazyByteString = BondPut . lift . B.putLazyByteString

glocal :: Monad m => (GetContext -> GetContext) -> BondGet t m a -> BondGet t m a
glocal f (BondGet g) = BondGet (R.local f g)

plocal :: Monad m => (PutContext -> PutContext) -> BondPut t m -> BondPut t m
plocal f (BondPut g) = BondPut (R.local f g)
