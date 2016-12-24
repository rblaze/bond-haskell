{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Network.Bond.CommClient where

import Data.Bond
import Data.Bond.Types

class CommClient m t where
    talk :: (BondStruct req, BondStruct resp) => t -> Utf8 -> Utf8 -> req -> m resp
    sendEvent :: BondStruct req => t -> Utf8 -> Utf8 -> req -> m ()
