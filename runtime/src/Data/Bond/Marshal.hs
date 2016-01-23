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
import Data.Bond.Utils

import Data.Bond.Schema.ProtocolType

import qualified Data.ByteString.Lazy as BL

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
