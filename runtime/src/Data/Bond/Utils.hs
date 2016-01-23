module Data.Bond.Utils where

import Data.Bond.Schema.ProtocolType

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BL

parseHeader :: BL.ByteString -> (ProtocolType, Word16)
parseHeader s | BL.length s /= 4 = (ProtocolType maxBound, maxBound)
parseHeader s = (protoSig, protoVer)
    where
    [s0, s1, v0, v1] = BL.unpack $ BL.take 4 s
    protoSig = ProtocolType $ fromIntegral s0 .|. (fromIntegral s1 `shiftL` 8)
    protoVer = fromIntegral v0 .|. (fromIntegral v1 `shiftL` 8)

protoHeader :: ProtocolType -> Word16 -> BL.ByteString
protoHeader (ProtocolType protoSig) protoVer = BL.pack [s0, s1, v0, v1]
    where
    s0 = fromIntegral protoSig
    s1 = fromIntegral (protoSig `shiftR` 8)
    v0 = fromIntegral protoVer
    v1 = fromIntegral (protoVer `shiftR` 8)

