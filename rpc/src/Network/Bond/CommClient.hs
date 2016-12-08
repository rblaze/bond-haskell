{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Network.Bond.CommClient where

import Control.Monad.IO.Class
import Data.Bond
import Data.Bond.Types

class CommClient m t where
    talk :: (BondStruct req, BondStruct resp) => Utf8 -> Utf8 -> t -> req -> m resp
    sendEvent :: BondStruct req => Utf8 -> Utf8 -> t -> req -> m ()

data EpoxyClient = EpoxyClient

instance MonadIO m => CommClient m EpoxyClient where
    talk = undefined
    sendEvent = undefined
