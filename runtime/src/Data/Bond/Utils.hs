{-# Language ScopedTypeVariables, TypeFamilies #-}
module Data.Bond.Utils where

import Data.Bond.Monads
import Data.Bond.Proto
import Data.Bond.Schema

import Control.Applicative
import Data.Bits
import Data.Proxy
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import qualified Control.Monad.Reader as R
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as L

getVarInt :: forall t a. (FiniteBits a, Num a) => BondGet t B.Get a
getVarInt = step 0
    where
    step n | n > finiteBitSize (0 :: a) `div` 7 = fail "VarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return 0
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

putVarInt :: (FiniteBits a, Integral a) => a -> BondPut t B.PutM
putVarInt i | i < 0 = error "VarInt with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = let iLow = fromIntegral $ i .&. 0x7F
               in do
                    putWord8 $ iLow `setBit` 7
                    putVarInt (i `shiftR` 7)

binaryDecode :: forall a t . (BondStruct a, BondProto t, ReaderM t ~ B.Get) => Proxy t -> L.ByteString -> Either String a
binaryDecode _ s =
    let BondGet g = bondGetStruct :: BondGet t B.Get a
        schema = getSchema (Proxy :: Proxy a)
     in case B.runGetOrFail (R.runReaderT g schema) s of
            Left (_, used, msg) -> Left $ "parse error at " ++ show used ++ ": " ++ msg
            Right (rest, used, _) | not (L.null rest) -> Left $ "incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)
            Right (_, _, a) -> Right a

binaryEncode :: forall a t . (BondStruct a, BondProto t, WriterM t ~ B.PutM) => Proxy t -> a -> Either String L.ByteString
binaryEncode _ a =
    let BondPut g = bondPutStruct a :: BondPut t B.PutM
        schema = getSchema (Proxy :: Proxy a)
     in Right $ B.runPut (R.runReaderT g (schema, error "Thou shalt not touch this"))
