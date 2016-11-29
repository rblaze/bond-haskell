module Network.Bond.CommClient where

import Control.Monad.IO.Class
import Data.Bond

class CommClient t where
    talk :: (MonadIO m, BondStruct req, BondStruct resp) => t -> req -> m resp
    sendEvend :: (MonadIO m, BondStruct req) => t -> req -> m ()
