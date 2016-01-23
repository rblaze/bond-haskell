{-# LANGUAGE MultiWayIf #-}
module Data.Bond.Marshal (
    bondMarshal,
    bondUnmarshal
  ) where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto

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

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondUnmarshal s
    = let (hdr, rest) = BL.splitAt 4 s
          (protoSig, protoVer) = parseHeader hdr
       in if | protoSig == fAST_PROTOCOL && protoVer == 1 -> bondRead FastBinaryProto rest
             | protoSig == cOMPACT_PROTOCOL && protoVer == 1 -> bondRead CompactBinaryV1Proto rest
             | protoSig == cOMPACT_PROTOCOL && protoVer == 2 -> bondRead CompactBinaryProto rest
             | protoSig == sIMPLE_PROTOCOL && protoVer == 1 -> bondRead SimpleBinaryV1Proto rest
             | protoSig == sIMPLE_PROTOCOL && protoVer == 2 -> bondRead SimpleBinaryProto rest
             | protoSig == sIMPLE_JSON_PROTOCOL && protoVer == 1 -> bondRead JsonProto rest
             | otherwise -> Left "unknown signature in marshalled stream"

class BondProto t => MarshalledWriter t where
    bondMarshal :: BondStruct a => t -> a -> BL.ByteString

instance MarshalledWriter FastBinaryProto where
    bondMarshal _ = BL.append (protoHeader fAST_PROTOCOL 1) . bondWrite FastBinaryProto

instance MarshalledWriter CompactBinaryV1Proto where
    bondMarshal _ = BL.append (protoHeader cOMPACT_PROTOCOL 1) . bondWrite CompactBinaryV1Proto

instance MarshalledWriter CompactBinaryProto where
    bondMarshal _ = BL.append (protoHeader cOMPACT_PROTOCOL 2) . bondWrite CompactBinaryProto

instance MarshalledWriter SimpleBinaryV1Proto where
    bondMarshal _ = BL.append (protoHeader sIMPLE_PROTOCOL 1) . bondWrite SimpleBinaryV1Proto

instance MarshalledWriter SimpleBinaryProto where
    bondMarshal _ = BL.append (protoHeader sIMPLE_PROTOCOL 2) . bondWrite SimpleBinaryProto

instance MarshalledWriter JsonProto where
    bondMarshal _ = BL.append (protoHeader sIMPLE_JSON_PROTOCOL 1) . bondWrite JsonProto
