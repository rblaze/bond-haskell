{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module Data.Bond.Monads where

import Data.Bond.Proto
import Data.Bond.Types

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- XXX until I can upgrade to binary >= 0.7.2.0 (and drop lts-2 support), isolate is unavailable
isolate :: ReaderM t ~ ReaderT c B.Get => Int -> BondGet t a -> BondGet t a
isolate _ = Prelude.id
--isolate n (BondGet g) = do
--    env <- ask
--    BondGet $ lift $ B.isolate n (runReaderT g env)

lookAhead :: ReaderM t ~ ReaderT c B.Get => BondGet t a -> BondGet t a
lookAhead (BondGet g) = do
    env <- ask
    BondGet $ lift $ B.lookAhead (runReaderT g env)

skip :: ReaderM t ~ ReaderT c B.Get => Int -> BondGet t ()
skip = BondGet . lift . B.skip

bytesRead :: ReaderM t ~ ReaderT c B.Get => BondGet t Int64
bytesRead = BondGet $ lift B.bytesRead

getWord8 :: ReaderM t ~ ReaderT c B.Get => BondGet t Word8
getWord8 = BondGet $ lift B.getWord8

getWord16le :: ReaderM t ~ ReaderT c B.Get => BondGet t Word16
getWord16le = BondGet $ lift B.getWord16le

getWord32le :: ReaderM t ~ ReaderT c B.Get => BondGet t Word32
getWord32le = BondGet $ lift B.getWord32le

getWord64le :: ReaderM t ~ ReaderT c B.Get => BondGet t Word64
getWord64le = BondGet $ lift B.getWord64le

getByteString :: ReaderM t ~ ReaderT c B.Get => Int -> BondGet t BS.ByteString
getByteString = BondGet . lift . B.getByteString

getLazyByteString :: ReaderM t ~ ReaderT c B.Get => Int64 -> BondGet t BL.ByteString
getLazyByteString = BondGet . lift . B.getLazyByteString

putWord8 :: WriterM t ~ ReaderT c B.PutM => Word8 -> BondPut t
putWord8 = BondPut . lift . B.putWord8

putWord16le :: WriterM t ~ ReaderT c B.PutM => Word16 -> BondPut t
putWord16le = BondPut . lift . B.putWord16le

putWord32le :: WriterM t ~ ReaderT c B.PutM => Word32 -> BondPut t
putWord32le = BondPut . lift . B.putWord32le

putWord64le :: WriterM t ~ ReaderT c B.PutM => Word64 -> BondPut t
putWord64le = BondPut . lift . B.putWord64le

putByteString :: WriterM t ~ ReaderT c B.PutM => BS.ByteString -> BondPut t
putByteString = BondPut . lift . B.putByteString

putLazyByteString :: WriterM t ~ ReaderT c B.PutM => BL.ByteString -> BondPut t
putLazyByteString = BondPut . lift . B.putLazyByteString
