{-# LANGUAGE MultiWayIf #-}
module Data.Bond.Marshal (
    bondUnmarshal
  ) where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto

import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondUnmarshal s
    = let (sig, rest) = BL.splitAt 4 s
       in if | sig == protoSig FastBinaryProto -> bondRead FastBinaryProto rest
             | sig == protoSig CompactBinaryProto -> bondRead CompactBinaryProto rest
             | sig == protoSig CompactBinaryV1Proto -> bondRead CompactBinaryV1Proto rest
             | sig == protoSig SimpleBinaryProto -> bondRead SimpleBinaryProto rest
             | sig == protoSig SimpleBinaryV1Proto -> bondRead SimpleBinaryV1Proto rest
             | sig == protoSig JsonProto -> bondRead JsonProto rest
             | otherwise -> Left "unknown signature in marshalled stream"
