module Data.Bond.SimpleBinaryProto where

import Data.Bond.Proto

data SimpleBinaryProto = SimpleBinaryProto
data SimpleBinaryV1Proto = SimpleBinaryV1Proto

instance BondProto SimpleBinaryProto
instance BondProto SimpleBinaryV1Proto
