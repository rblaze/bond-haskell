{-# LANGUAGE NamedFieldPuns #-}
module Network.Bond.Internal.Epoxy.Frame where 

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Binary.Put
import Data.Bond.Types
import Data.List
import Prelude          -- ghc 7.10 workaround for Control.Applicative
import qualified Data.ByteString.Lazy as BL

import Network.Bond.Internal.CommSocket

data FrameletType
    = EpoxyConfig
    | EpoxyHeaders
    | ErrorData
    | LayerData
    | PayloadData
    | ProtocolError
    deriving (Show, Eq)

frameletTypeToWord :: FrameletType -> Word16
frameletTypeToWord EpoxyConfig = 0x4743
frameletTypeToWord EpoxyHeaders = 0x5248
frameletTypeToWord ErrorData = 0x4445
frameletTypeToWord LayerData = 0x594C
frameletTypeToWord PayloadData = 0x4450
frameletTypeToWord ProtocolError = 0x5245

wordToFrameletType :: Word16 -> Maybe FrameletType
wordToFrameletType 0x4743 = Just EpoxyConfig 
wordToFrameletType 0x5248 = Just EpoxyHeaders 
wordToFrameletType 0x4445 = Just ErrorData 
wordToFrameletType 0x594C = Just LayerData 
wordToFrameletType 0x4450 = Just PayloadData 
wordToFrameletType 0x5245 = Just ProtocolError 
wordToFrameletType _ = Nothing

data Framelet = Framelet { flType :: FrameletType, flData :: BL.ByteString }

data Frame = Frame { framelets :: [Framelet] }

sendFrame :: (MonadIO m, CommSocket s) => SmartSocket s -> Frame -> m ()
sendFrame _ Frame{framelets = []} = liftIO $ ioError $ userError "internal error: attempt to send empty frame"
sendFrame s Frame{framelets} = do
    let output = runPut $ do
            putWord16le $ genericLength framelets
            forM_ framelets $ \Framelet{ flType, flData } -> do
                putWord16le $ frameletTypeToWord flType
                putWord32le $ fromIntegral $ BL.length flData
                putLazyByteString flData
    liftIO $ sockSend s output

recvFrame :: (MonadIO m, CommSocket s) => SmartSocket s -> m Frame
recvFrame s = do
    let decoder = runGetIncremental $ do
            nframelets <- getWord16le
            -- FIXME check range, fail if incorrect
            fls <- replicateM (fromIntegral nframelets) $ do
                fltype <- wordToFrameletType <$> getWord16le
                case fltype of
                    Nothing -> fail "Invalid framelet type"
                    Just ft -> do
                        flsize <- getWord32le
                        fldata <- getLazyByteString $ fromIntegral flsize
                        return Framelet{ flType = ft, flData = fldata }
            return Frame{ framelets = fls }
    let loop d = case d of
            Fail _ _ e -> do
                -- FIXME send protocol error, close connection
                liftIO $ ioError $ userError e
            Done rest _ a -> do
                sockPushBack s rest
                return a
            Partial f -> do
                bs <- sockRecv s
                -- FIXME check for EOF (should never happen)
                loop $ f (Just bs)
    loop decoder
