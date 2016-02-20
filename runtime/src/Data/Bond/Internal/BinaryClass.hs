{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, InstanceSigs, GADTs #-}
module Data.Bond.Internal.BinaryClass where

import Control.Monad.Error
import Data.Word
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

class Monad m => BinaryPut m where
    tryPut :: m () -> Either String BL.ByteString
    putWord8 :: Word8 -> m ()
    putWord16le :: Word16 -> m ()
    putWord32le :: Word32 -> m ()
    putWord64le :: Word64 -> m ()
    putByteString :: BS.ByteString -> m ()
    putLazyByteString :: BL.ByteString -> m ()

instance BinaryPut B.PutM where
    tryPut = Right . B.runPut
    putWord8 = B.putWord8
    putWord16le = B.putWord16le
    putWord32le = B.putWord32le
    putWord64le = B.putWord64le
    putByteString = B.putByteString
    putLazyByteString = B.putLazyByteString

instance BinaryPut (ErrorT String B.PutM) where
    tryPut g = case B.runPutM (runErrorT g) of
                (Left msg, _) -> Left msg
                (Right (), bs) -> Right bs
    putWord8 = lift . putWord8
    putWord16le = lift . putWord16le
    putWord32le = lift . putWord32le
    putWord64le = lift . putWord64le
    putByteString = lift . putByteString
    putLazyByteString = lift . putLazyByteString
