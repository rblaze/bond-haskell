module Data.Bond.Marshal where

import {-# SOURCE #-} Data.Bond.Bonded
import {-# SOURCE #-} Data.Bond.Proto
import Data.Bond.Struct
import {-# SOURCE #-} Data.Bond.Schema.SchemaDef
import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
bondRecode :: (BondProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
bondRecodeStruct :: BondProto t => t -> SchemaDef -> Bonded Struct -> Either String (Bonded Struct)
bondRecodeToTagged :: (BondTaggedProto t, BondStruct a) => t -> Bonded a -> Either String (Bonded a)
