module Network.Bond.Internal.CommSocket
    ( CommSocket(..)
    , SmartSocket
    , sockClose
    , sockCreate
    , sockPushBack
    , sockRecv
    , sockSend
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

class CommSocket s where
    bsClose :: MonadIO m => s -> m ()
    bsSend :: MonadIO m => s -> BL.ByteString -> m ()
    bsRecv :: MonadIO m => s -> m BS.ByteString

data SmartSocket s = SmartSocket
    { sock :: s
    , buf :: MVar BS.ByteString
    }

sockCreate :: MonadIO m => s -> m (SmartSocket s)
sockCreate s = do
    emptyBuf <- liftIO newEmptyMVar
    return SmartSocket{ sock = s, buf = emptyBuf }

sockClose :: (MonadIO m, CommSocket s) => SmartSocket s -> m ()
sockClose = bsClose . sock

sockSend :: (MonadIO m, CommSocket s) => SmartSocket s -> BL.ByteString -> m ()
sockSend s = bsSend (sock s)

sockRecv :: (MonadIO m, CommSocket s) => SmartSocket s -> m BS.ByteString
sockRecv s = do
    stored <- liftIO $ tryTakeMVar (buf s)
    case stored of
        Just v -> return v
        Nothing -> bsRecv (sock s)

sockPushBack :: MonadIO m => SmartSocket s -> BS.ByteString -> m ()
sockPushBack s bs = liftIO $ do
    isEmpty <- isEmptyMVar $ buf s
    unless isEmpty $ ioError $ userError "internal error: pushing data to unread buffer"
    putMVar (buf s) bs
