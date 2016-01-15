module Data.Bond.Marshal where

import Data.Bond.CompactBinaryProto
import Data.Bond.FastBinaryProto
import Data.Bond.JsonProto
import Data.Bond.Proto
import Data.Bond.SimpleBinaryProto

import Control.Applicative
import qualified Data.ByteString.Lazy as L

bondReadMarshalled :: BondStruct a => L.ByteString -> Either String a
bondReadMarshalled s
    = case result of
        IncorrectSignature -> Left "unknown protocol"
        DecodeError msg -> Left msg
        Decoded a -> Right a
    where
    result = bondDecodeMarshalled CompactBinaryProto s
            <|> bondDecodeMarshalled FastBinaryProto s
            <|> bondDecodeMarshalled SimpleBinaryProto s
            <|> bondDecodeMarshalled JsonProto s
            <|> bondDecodeMarshalled CompactBinaryV1Proto s
            <|> bondDecodeMarshalled SimpleBinaryV1Proto s

bondWriteMarshalled :: (BondStruct a, BondProto t) => t -> a -> Either String L.ByteString
bondWriteMarshalled = bondEncodeMarshalled
