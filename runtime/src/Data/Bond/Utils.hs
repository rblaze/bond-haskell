module Data.Bond.Utils where

import Data.Bond.Proto
import Data.Bond.Schema.ProtocolType

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BS

protoHeader :: ProtocolType -> Word16 -> BS.ByteString
protoHeader (ProtocolType protoSig) protoVer = BS.pack [s0, s1, v0, v1]
    where
    s0 = fromIntegral protoSig
    s1 = fromIntegral (protoSig `shiftR` 8)
    v0 = fromIntegral protoVer
    v1 = fromIntegral (protoVer `shiftR` 8)

{-
decodeWithHdr :: (BondProto t, BondStruct a) => ProtocolType -> Word16 -> t -> BS.ByteString -> MarshalledDecodeResult a
decodeWithHdr protoSig protoVer p s =
    if BS.take 4 s == protoHeader protoSig protoVer
        then case bondDecode p (BS.drop 4 s) of
                Left msg -> DecodeError msg
                Right a -> Decoded a
        else IncorrectSignature

encodeWithHdr :: (BondProto t, BondStruct a) => ProtocolType -> Word16 -> t -> a -> Either String BS.ByteString
encodeWithHdr protoSig protoVer p = fmap (BS.append $ protoHeader protoSig protoVer) . bondEncode p
-}
