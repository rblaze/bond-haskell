module Data.Bond.Marshal where

import {-# SOURCE #-} Data.Bond.Bonded
import {-# SOURCE #-} Data.Bond.Proto
import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondRecode :: (BondProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
bondRecodeToTagged :: (BondTaggedProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
