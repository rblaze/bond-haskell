{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, BangPatterns, MultiWayIf #-}
module Data.Bond.Internal.BinaryUtils where

import Data.Bond.Types
import Data.Bond.Internal.Protocol

import Control.Applicative
import Control.Monad.Error
import Data.Bits
import Data.Monoid
import Prelude -- workaround for Control.Applicative in ghc 7.10
import qualified Data.Binary.Builder as BLD
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- XXX isolate is available in binary >= 0.7.2.0 (lts-3 and higher) only
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
getVarInt = BondGet $ fastGet <|> slowGet 0
    where
    maxSize = 1 + finiteBitSize (0 :: a) `div` 7
    fastGet = do
        binstr <- B.lookAhead $ B.getByteString maxSize
        let loop i !v =
                let b = fromIntegral $ BS.index binstr i
                    v' = v .|. ((b `clearBit` 7) `shiftL` (i * 7))
                 in if | i >= maxSize -> Nothing
                       | b `testBit` 7 -> loop (i + 1) v'
                       | otherwise -> Just (i + 1, v')
        case loop 0 0 of
            Nothing -> fail "VarInt: sequence too long"
            Just (consumed, value) -> do
                B.skip consumed
                return value
    -- fallback in case there is not enough bytes in stream for max sequence
    slowGet i | i >= maxSize = fail "VarInt: sequence too long"
    slowGet i = do
        b <- fromIntegral <$> B.getWord8
        rest <- if b `testBit` 7 then slowGet (i + 1)  else return 0
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
putVarInt n | n < 0 = error "VarInt with negative value"
putVarInt n = BondPut $ lift $ B.putBuilder $ makeBuilder n
    where
    makeBuilder i | i < 128 = BLD.singleton $ fromIntegral i
    makeBuilder i | i < 16384 = let (b1, b0) = fromIntegral i `divMod` 128
                                 in BLD.putWord16be $ 0x8000 .|. (b0 `shiftL` 8) .|. b1
    makeBuilder i | i < 2097152 = let (temp, b0) = i `divMod` 128
                                      (b2, b1) = temp `divMod` 128
                                   in BLD.putWord16be (0x8080 .|. fromIntegral ((b0 `shiftL` 8) .|. b1)) <>
                                        BLD.singleton (fromIntegral b2)
    makeBuilder i | i < 268435456 = let (temp1, b0) = i `divMod` 128
                                        (temp2, b1) = temp1 `divMod` 128
                                        (b3, b2) = temp2 `divMod` 128
                                     in BLD.putWord32be $ 0x80808000 .|.
                                            fromIntegral ((b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3)
    makeBuilder i = let lo = fromIntegral $ i .&. 0x7F
                     in BLD.singleton (lo .|. 0x80) <> makeBuilder (i `shiftR` 7)
