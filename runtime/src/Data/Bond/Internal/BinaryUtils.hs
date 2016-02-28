{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module Data.Bond.Internal.BinaryUtils where

import Data.Bond.Types
import Data.Bond.Internal.Protocol

import Control.Applicative
import Control.Monad.Error
import Data.Bits
import Prelude -- workaround for Control.Applicative in ghc 7.10
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- XXX until I can upgrade to binary >= 0.7.2.0 (and drop lts-2 support), isolate is unavailable
isolate :: ReaderM t ~ B.Get => Int -> BondGet t a -> BondGet t a
isolate _ = Prelude.id
--isolate n (BondGet g) = BondGet $ B.isolate n g

lookAhead :: ReaderM t ~ B.Get => BondGet t a -> BondGet t a
lookAhead (BondGet g) = BondGet $ B.lookAhead g

skip :: ReaderM t ~ B.Get => Int -> BondGet t ()
skip = BondGet . B.skip

bytesRead :: ReaderM t ~ B.Get => BondGet t Int64
bytesRead = BondGet B.bytesRead

getWord8 :: ReaderM t ~ B.Get => BondGet t Word8
getWord8 = BondGet B.getWord8

getWord16le :: ReaderM t ~ B.Get => BondGet t Word16
getWord16le = BondGet B.getWord16le

getWord32le :: ReaderM t ~ B.Get => BondGet t Word32
getWord32le = BondGet B.getWord32le

getWord64le :: ReaderM t ~ B.Get => BondGet t Word64
getWord64le = BondGet B.getWord64le

getByteString :: ReaderM t ~ B.Get => Int -> BondGet t BS.ByteString
getByteString = BondGet . B.getByteString

getLazyByteString :: ReaderM t ~ B.Get => Int64 -> BondGet t BL.ByteString
getLazyByteString = BondGet . B.getLazyByteString

getVarInt :: forall a t. (FiniteBits a, Num a, ReaderM t ~ B.Get) => BondGet t a
getVarInt = step 0
    where
    step n | n > finiteBitSize (0 :: a) `div` 7 = fail "VarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return 0
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

tryPut :: ErrorT String B.PutM () -> Either String BL.ByteString
tryPut g = case B.runPutM (runErrorT g) of
            (Left msg, _) -> Left msg
            (Right (), bs) -> Right bs

putWord8 :: WriterM t ~ ErrorT String B.PutM => Word8 -> BondPut t
putWord8 = BondPut . lift . B.putWord8

putWord16le :: WriterM t ~ ErrorT String B.PutM => Word16 -> BondPut t
putWord16le = BondPut . lift . B.putWord16le

putWord32le :: WriterM t ~ ErrorT String B.PutM => Word32 -> BondPut t
putWord32le = BondPut . lift . B.putWord32le

putWord64le :: WriterM t ~ ErrorT String B.PutM => Word64 -> BondPut t
putWord64le = BondPut . lift . B.putWord64le

putByteString :: WriterM t ~ ErrorT String B.PutM => BS.ByteString -> BondPut t
putByteString = BondPut . lift . B.putByteString

putLazyByteString :: WriterM t ~ ErrorT String B.PutM => BL.ByteString -> BondPut t
putLazyByteString = BondPut . lift . B.putLazyByteString

putVarInt :: (FiniteBits a, Integral a, WriterM t ~ ErrorT String B.PutM) => a -> BondPut t
putVarInt i | i < 0 = error "VarInt with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = let iLow = fromIntegral $ i .&. 0x7F
               in do
                    putWord8 $ iLow `setBit` 7
                    putVarInt (i `shiftR` 7)
