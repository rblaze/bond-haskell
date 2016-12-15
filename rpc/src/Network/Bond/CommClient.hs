{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Network.Bond.CommClient where

import Data.Bond
import Data.Bond.Types

class CommClient m t where
    talk :: (BondStruct req, BondStruct resp) => Utf8 -> Utf8 -> t -> req -> m resp
    sendEvent :: BondStruct req => Utf8 -> Utf8 -> t -> req -> m ()
