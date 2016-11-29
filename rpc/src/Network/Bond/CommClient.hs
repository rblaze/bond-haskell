{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Network.Bond.CommClient where

import Control.Monad.IO.Class
import Data.Bond

class MonadIO m => CommClient m t where
    talk :: (BondStruct req, BondStruct resp) => t -> req -> m resp
    sendEvent :: BondStruct req => t -> req -> m ()

data EpoxyClient = EpoxyClient

instance MonadIO m => CommClient m EpoxyClient where
    talk = undefined
    sendEvent = undefined
