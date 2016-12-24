module Network.Bond.Internal.Epoxy.ProtocolException where

import Control.Exception.Safe

data ProtocolException
    = InvalidConfig             -- ^ Error while negotiating connection options
    | InvalidFrameFormat        -- XXX split to truncated frame, etc
    | ProtocolErrorReceived     -- ^ Peer reported error
    | ProtocolViolation         -- XXX split to invalid frame type, unknown framelet, etc
    | InvalidPayload            -- Payload de/serialization error
    deriving (Show, Typeable)

instance Exception ProtocolException
