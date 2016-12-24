{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns, ScopedTypeVariables, LambdaCase #-}
module Network.Bond.Internal.Epoxy.Client
    ( EpoxyClient
    , createEpoxyClient
    --, closeEpoxyClient
    ) where

--import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Base (AssertionFailed(..))
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Bond
import Data.Bond.Types
import Network.Socket hiding (connect)
--import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL

import Network.Bond.CommClient
import Network.Bond.Internal.Epoxy.Frame
import Network.Bond.Internal.Epoxy.ProtocolException
import Network.Bond.Internal.CommSocket
import Network.Bond.Internal.CommSocket.Net

-- import Bond.Comm.Error (Error)
import Bond.Comm.Epoxy.EpoxyHeaders hiding (EpoxyHeaders)
import Bond.Comm.Epoxy.EpoxyMessageType
-- import Bond.Comm.Epoxy.ProtocolErrorCode
import qualified Bond.Comm.Epoxy.EpoxyConfig as Cfg
import qualified Bond.Comm.Epoxy.EpoxyHeaders as Hdr
-- import qualified Bond.Comm.Epoxy.ProtocolError as PE

type ConvId = Word64
data FrameToSend = QueuedFrame (Maybe ConvId) Frame
data ResponseStatus
    = Pending
    | Received BL.ByteString
    | Error SomeException

data EpoxyClient s = EpoxyClient
    { ecAddr :: SockAddr
    , ecSock :: TVar (SmartSocket s)
    , ecConvId :: TVar ConvId
    , ecSendQueue :: TQueue FrameToSend
    , ecPending :: TVar (HM.HashMap ConvId ResponseStatus)
    , ecSendThread :: Async ()
    , ecRecvThread :: Async ()
    }

instance MonadIO m => CommClient m (EpoxyClient s) where
    talk = epoxyTalk
    sendEvent = epoxySendEvent

-- public API
createEpoxyClient :: MonadIO m => SockAddr -> m (EpoxyClient TCPSocket)
createEpoxyClient addr = do
    sock <- epoxyConnect addr
    (sockvar, conv, queue, pending) <- liftIO $ atomically $ do
        s <- newTVar sock
        c <- newTVar 1
        q <- newTQueue
        p <- newTVar HM.empty
        return (s, c, q, p)
    sendThread <- liftIO $ async $ epoxySendThread addr sockvar queue pending
    recvThread <- liftIO $ async $ epoxyRecvThread sockvar pending
    return EpoxyClient
        { ecAddr = addr
        , ecSock = sockvar
        , ecConvId = conv
        , ecSendQueue = queue
        , ecPending = pending
        , ecSendThread = sendThread
        , ecRecvThread = recvThread
        }

-- createEpoxyTlsClient :: MonadIO m => m EpoxyClient
-- createEpoxyTlsClient = return EpoxyClient

epoxyTalk :: (MonadIO m, BondStruct req, BondStruct resp) => EpoxyClient s -> Utf8 -> Utf8 -> req -> m resp
epoxyTalk client service method req = liftIO $ do
    -- XXX on error send error frame, close socket
    -- enqueue frame
    payload <- case bondMarshal CompactBinaryV1Proto req of
        Left _ -> throw InvalidPayload
        Right blob -> return blob
    convId <- atomically $ sendPayload client service method rEQUEST payload
    -- wait for response frame
    respBlob <- atomically $ recvPayload client convId
    case bondUnmarshal respBlob of
        Left _ -> throw InvalidPayload
        Right resp ->  return resp
    

epoxySendEvent :: (MonadIO m, BondStruct req) => EpoxyClient s -> Utf8 -> Utf8 -> req -> m ()
epoxySendEvent client service method req = liftIO $ do
    payload <- case bondMarshal CompactBinaryV1Proto req of
        Left _ -> throw InvalidPayload
        Right blob -> return blob
    void $ atomically $ sendPayload client service method eVENT payload

epoxyConnect :: MonadIO m => SockAddr -> m (SmartSocket TCPSocket)
epoxyConnect addr = liftIO $
    bracketOnError
    (connect addr)
    sockClose $    -- FIXME send ProtocolError frame
    \sock -> do
        framedata <- case bondWrite FastBinaryProto (defaultValue :: Cfg.EpoxyConfig) of
            Left msg -> throw $ AssertionFailed $ "internal error in config frame: " ++ msg
            Right blob -> return blob

        sendFrame sock $ Frame [Framelet{ flType = EpoxyConfig, flData = framedata }]
        serverResponse <- recvFrame sock
        -- no config options defined yet, so just check there is single config framelet
        case serverResponse of
            Frame [Framelet{ flType = EpoxyConfig, flData }] ->
                case bondRead FastBinaryProto flData of
                    Left _ -> throw InvalidFrameFormat
                    Right (_ :: Cfg.EpoxyConfig) -> return ()
            _ -> throw ProtocolViolation

        return sock

sendPayload :: EpoxyClient s -> Utf8 -> Utf8 -> EpoxyMessageType -> BL.ByteString -> STM ConvId
sendPayload client service method msgtype payload = do
    convId <- readTVar (ecConvId client)
    writeTVar (ecConvId client) (convId + 2)
    let hdr = Hdr.EpoxyHeaders
            { conversation_id = convId
            , message_type = msgtype
            , service_name = service
            , method_name = method
            }
    hdrdata <- case bondWrite FastBinaryProto hdr of
        Left msg -> throw $ AssertionFailed $ "internal error in headers frame: " ++ msg
        Right blob -> return blob
    let respConvId = if msgtype == eVENT then Nothing else Just convId
    writeTQueue (ecSendQueue client) $ QueuedFrame respConvId $ Frame
        [ Framelet{ flType = EpoxyHeaders, flData = hdrdata }
        , Framelet{ flType = PayloadData, flData = payload }
        ]
    return convId

epoxySendThread :: SockAddr -> TVar (SmartSocket TCPSocket) -> TQueue FrameToSend -> TVar (HM.HashMap ConvId ResponseStatus) -> IO ()
epoxySendThread addr sockvar queue pending = do
    sock <- atomically $ readTVar sockvar
    loop sock
    where
    loop sock = do
        frame <- atomically $ do
            QueuedFrame convId frame <- readTQueue queue
            withJust convId $ \v -> modifyTVar' pending $ HM.insert v Pending
            return frame
        sendFrame sock frame `withException` \e ->
            -- FIXME add socket shutdown/reconnect on error
            atomically $ modifyTVar pending $ HM.map $ \case
                Pending -> Error e
                v -> v
        loop sock
    withJust a f = maybe (return ()) f a

recvPayload :: EpoxyClient s -> ConvId -> STM BL.ByteString
recvPayload client convId = do
    resps <- readTVar (ecPending client)
    case HM.lookup convId resps of
        Nothing -> throw $ AssertionFailed "internal error: no response slot"
        Just Pending -> retry
        Just (Error e) -> throw e
        Just (Received f) -> do
            modifyTVar (ecPending client) $ HM.delete convId
            return f

epoxyRecvThread :: TVar (SmartSocket TCPSocket) -> TVar (HM.HashMap ConvId ResponseStatus) -> IO ()
epoxyRecvThread sockvar pending = loop
    where
    -- FIXME handle errors, signal need to send protocol error and reopen socket
    loop = do
        sock <- atomically $ readTVar sockvar
        Frame framelets <- recvFrame sock
        when (null framelets) $ throw ProtocolViolation
        let headers = head framelets
        convId <- case flType headers of
            EpoxyHeaders -> do
                case bondRead FastBinaryProto (flData $ head framelets) of
                    Left _ -> throw InvalidFrameFormat
                    Right hdr -> do
                        unless (message_type hdr == rESPONSE) $ throw ProtocolViolation
                        return $ conversation_id hdr
            ProtocolError -> throw ProtocolErrorReceived
            _ -> throw ProtocolViolation
        let pd = last framelets
        case flType pd of
            PayloadData -> atomically $ do
                resps <- readTVar pending
                case HM.lookup convId resps of
                    Just Pending -> modifyTVar pending $ HM.insert convId (Received $ flData pd)
                    _ -> throw ProtocolViolation -- FIXME unexpected convId
            ErrorData ->
                return () -- FIXME return server-side error. It's polymorphic :(
            _ -> throw ProtocolViolation
        loop
