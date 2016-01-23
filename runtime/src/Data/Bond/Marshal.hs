{-# LANGUAGE MultiWayIf #-}
module Data.Bond.Marshal (
    bondUnmarshal
  ) where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto
import Data.Bond.Utils

import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondUnmarshal s
    = let (hdr, rest) = BL.splitAt 4 s
          (protoSig, protoVer) = parseHeader hdr
       in if | protoSig == protoSignature FastBinaryProto && protoVer == protoVersion FastBinaryProto -> bondRead FastBinaryProto rest
             | protoSig == protoSignature CompactBinaryProto && protoVer == protoVersion CompactBinaryProto -> bondRead CompactBinaryProto rest
             | protoSig == protoSignature CompactBinaryV1Proto && protoVer == protoVersion CompactBinaryV1Proto -> bondRead CompactBinaryV1Proto rest
             | protoSig == protoSignature SimpleBinaryProto && protoVer == protoVersion SimpleBinaryProto -> bondRead SimpleBinaryProto rest
             | protoSig == protoSignature SimpleBinaryV1Proto && protoVer == protoVersion SimpleBinaryV1Proto -> bondRead SimpleBinaryV1Proto rest
             | protoSig == protoSignature JsonProto && protoVer == protoVersion JsonProto -> bondRead JsonProto rest
             | otherwise -> Left "unknown signature in marshalled stream"
