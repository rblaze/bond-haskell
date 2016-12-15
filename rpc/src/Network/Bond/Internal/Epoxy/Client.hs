{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns, LambdaCase #-}
module Network.Bond.Internal.Epoxy.Client
    ( EpoxyClient
    , createEpoxyClient
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Bond
import Data.Bond.Types
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.HashMap.Strict as HM

import Network.Bond.CommClient
import Network.Bond.Internal.Epoxy.Frame
import Network.Bond.Internal.CommSocket

import Bond.Comm.Epoxy.EpoxyHeaders hiding (EpoxyHeaders)
import Bond.Comm.Epoxy.EpoxyMessageType
import qualified Bond.Comm.Epoxy.EpoxyConfig as Cfg

type ConvId = Word64

data EpoxyClient s = EpoxyClient
    { ecSock :: SmartSocket s
    , ecRecvFlag :: TMVar ()
    , ecExpectedReplies :: TVar (HM.HashMap ConvId (Maybe Frame))
    , ecSendFlag :: MVar ()
    , ecNextConversationId :: MVar ConvId
    }

instance (MonadIO m, CommSocket s) => CommClient m (EpoxyClient s) where
    talk = epoxyTalk
    sendEvent = epoxySendEvent

createEpoxyClient :: (MonadIO m, CommSocket s) => SmartSocket s -> m (EpoxyClient s)
createEpoxyClient sock = liftIO $ do
    let config = bondWrite FastBinaryProto (defaultValue :: Cfg.EpoxyConfig)
    framedata <- case config of
        Left msg -> ioError $ userError $ "internal error in handshake: " ++ msg
        Right blob -> return blob
    sendFrame sock $ Frame [Framelet{ flType = EpoxyConfig, flData = framedata }]
    serverResponse <- recvFrame sock
    -- FIXME check server response
    rflag <- atomically $ newTMVar ()
    replies <- atomically $ newTVar HM.empty
    sflag <- newMVar ()
    conv <- newMVar 1
    let client = EpoxyClient
            { ecSock = sock
            , ecRecvFlag = rflag
            , ecExpectedReplies = replies
            , ecSendFlag = sflag
            , ecNextConversationId = conv
            }
    return client

sendPayload :: (CommSocket s, BondStruct req) => Utf8 -> Utf8 -> EpoxyClient s -> ConvId -> req -> IO ()
sendPayload service method client convid req = do
    -- prepare headers framelet
    let hdr = defaultValue
            { conversation_id = convid
            , message_type = eVENT
            , service_name = service
            , method_name = method
            }
    hdrblob <- case bondWrite FastBinaryProto hdr of
        Left msg -> ioError $ userError $ "internal error serializing headers: " ++ msg
        Right blob -> return blob
    -- prepare payload framelet
    payload <- case bondMarshal CompactBinaryV1Proto req of
        Left msg -> ioError $ userError $ "error serializing data: " ++ msg
        Right blob -> return blob

    -- lock socket for send and dispatch frame.
    -- MVar guarantees everyone will have a chance to send
    withMVar (ecSendFlag client) $ \_ -> do
        sendFrame (ecSock client) $ Frame
            [ Framelet{ flType = EpoxyHeaders, flData = hdrblob }
            , Framelet{ flType = PayloadData, flData = payload }
            ]

epoxyTalk :: (MonadIO m, CommSocket s, BondStruct req, BondStruct resp) => Utf8 -> Utf8 -> EpoxyClient s -> req -> m resp
epoxyTalk service method client req = liftIO $ do
    -- get new conversation id
    convid <- modifyMVar (ecNextConversationId client) $ \i -> return (i + 2, i)

    -- register interest in reply
    frame <- bracket_
        (atomically $ modifyTVar' replies $ HM.insert convid Nothing)
        (atomically $ modifyTVar' replies $ HM.delete convid) $
      do
        -- send request
        sendPayload service method client convid req

        bracket
            (getFrameOrLock convid)
            releaseLock $
            \case
                Just f -> return f
                Nothing -> recvLoop convid

    -- decode and return answer
    return defaultValue
    where
    replies = ecExpectedReplies client
    getFrameOrLock convid = atomically $ do
        v <- HM.lookup convid <$> readTVar replies
        case v of
            Nothing -> throwSTM $ userError "internal error: converation not registered"
            Just frame@(Just _) ->
                -- other thread already read our reply
                return frame
            Just Nothing -> do
                -- try to get recv lock
                takeTMVar (ecRecvFlag client)
                return Nothing
    releaseLock Nothing = atomically $ putTMVar (ecRecvFlag client) ()
    releaseLock _ = return ()
    recvLoop convid = do
        frame <- recvFrame (ecSock client)
        -- FIXME handle protocol errors
        let Frame [headers, payload] = frame
        hdr <- case bondRead FastBinaryProto (flData headers) of
            Left msg -> ioError $ userError $ "internal error serializing headers: " ++ msg
            Right h -> return h
        -- FIXME handle protocol errors
        if conversation_id hdr == convid
            then return frame
            else do
                atomically $ modifyTVar' replies $ HM.insert convid (Just frame)
                recvLoop convid

epoxySendEvent :: (MonadIO m, CommSocket s, BondStruct msg) => Utf8 -> Utf8 -> EpoxyClient s -> msg -> m ()
epoxySendEvent service method client msg = liftIO $ do
    -- get new conversation id
    convid <- modifyMVar (ecNextConversationId client) $ \i -> return (i + 2, i)
    -- send request
    sendPayload service method client convid msg
