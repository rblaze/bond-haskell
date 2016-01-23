module Data.Bond.Marshal where

import {-# SOURCE #-} Data.Bond.Proto
import qualified Data.ByteString.Lazy as BL

bondUnmarshal :: BondStruct a => BL.ByteString -> Either String a
