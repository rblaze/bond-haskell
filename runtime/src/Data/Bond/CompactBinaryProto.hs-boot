module Data.Bond.CompactBinaryProto where

import Data.Bond.Proto

data CompactBinaryProto = CompactBinaryProto
data CompactBinaryV1Proto = CompactBinaryV1Proto

instance BondProto CompactBinaryProto
instance BondTaggedProto CompactBinaryProto
instance BondProto CompactBinaryV1Proto
instance BondTaggedProto CompactBinaryV1Proto
