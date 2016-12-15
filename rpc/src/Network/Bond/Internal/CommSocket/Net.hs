module Network.Bond.Internal.CommSocket.Net
    ( TCPSocket
    , connect
    ) where

import Control.Monad.IO.Class
import Network.Bond.Internal.CommSocket
import Network.Socket hiding (connect, close, recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as S

newtype TCPSocket = TCPSocket Socket

instance CommSocket TCPSocket where
    bsClose (TCPSocket s) = liftIO $ S.close s
    bsSend (TCPSocket s) bs = liftIO $ sendMany s $ BL.toChunks bs
    bsRecv (TCPSocket s) = liftIO $ recv s 65536
        

mkTCPSocket :: MonadIO m => Family -> SockAddr -> m (SmartSocket TCPSocket)
mkTCPSocket family addr = liftIO $ do
    s <- socket family Stream defaultProtocol
    S.connect s addr
    sockCreate (TCPSocket s)

connect :: MonadIO m => SockAddr -> m (SmartSocket TCPSocket)
connect addr@SockAddrInet{} = mkTCPSocket AF_INET addr
connect addr@SockAddrInet6{} = mkTCPSocket AF_INET6 addr
connect _ = liftIO $ ioError $ userError "unsupported address type"
