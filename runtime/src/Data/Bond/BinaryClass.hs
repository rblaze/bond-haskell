module Data.Bond.BinaryClass where

import Control.Monad.Error
--import Data.Int
import Data.Word
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

class Monad m => BinaryPut m where
    putWord8 :: Word8 -> m ()
    putWord16le :: Word16 -> m ()
    putWord32le :: Word32 -> m ()
    putWord64le :: Word64 -> m ()
    putByteString :: BS.ByteString -> m ()
    putLazyByteString :: BL.ByteString -> m ()

instance BinaryPut B.PutM where
    putWord8 = B.putWord8
    putWord16le = B.putWord16le
    putWord32le = B.putWord32le
    putWord64le = B.putWord64le
    putByteString = B.putByteString
    putLazyByteString = B.putLazyByteString

instance (BinaryPut m, Error e) => BinaryPut (ErrorT e m) where
    putWord8 = lift . putWord8
    putWord16le = lift . putWord16le
    putWord32le = lift . putWord32le
    putWord64le = lift . putWord64le
    putByteString = lift . putByteString
    putLazyByteString = lift . putLazyByteString
