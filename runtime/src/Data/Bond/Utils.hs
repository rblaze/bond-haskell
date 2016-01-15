module Data.Bond.Utils where

import Data.Bond.Proto
import Data.Bond.Schema.ProtocolType

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BS

decodeWithHdr :: (BondProto t, BondStruct a) => ProtocolType -> Word16 -> t -> BS.ByteString -> MarshalledDecodeResult a
decodeWithHdr (ProtocolType protoSig) protoVer p s =
    if BS.length s >= 4 && sig == fromIntegral protoSig && ver == protoVer
        then case bondDecode p (BS.drop 4 s) of
                Left msg -> DecodeError msg
                Right a -> Decoded a
        else IncorrectSignature
    where
    [s0, s1, v0, v1] = map fromIntegral $ BS.unpack $ BS.take 4 s
    sig = (s1 `shiftL` 8) .|. s0
    ver = (v1 `shiftL` 8) .|. v0

encodeWithHdr :: (BondProto t, BondStruct a) => ProtocolType -> Word16 -> t -> a -> Either String BS.ByteString
encodeWithHdr (ProtocolType protoSig) protoVer p = fmap (BS.append hdr) . bondEncode p
    where
    hdr = BS.pack [s0, s1, v0, v1]
    s0 = fromIntegral protoSig
    s1 = fromIntegral (protoSig `shiftR` 8)
    v0 = fromIntegral protoVer
    v1 = fromIntegral (protoVer `shiftR` 8)
