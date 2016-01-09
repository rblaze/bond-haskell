{-# Language ScopedTypeVariables #-}
module Data.Bond.Utils where

import Control.Applicative
import Control.Monad
import Data.Bits
import Prelude          -- ghc 7.10 workaround for Control.Applicative

import Data.Binary.Get
import Data.Binary.Put

getVarInt :: forall a. (FiniteBits a, Num a) => Get a
getVarInt = step 0
    where
    step n | n > finiteBitSize (0 :: a) `div` 7 = fail "VarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return 0
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

putVarInt :: (FiniteBits a, Integral a) => a -> Put
putVarInt i | i < 0 = error "VarInt with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = let iLow = fromIntegral $ i .&. 0x7F
               in do
                    putWord8 $ iLow `setBit` 7
                    putVarInt (i `shiftR` 7)

checkSchemaMismatch :: (Eq a, Show a, Monad f) => a -> a -> f ()
checkSchemaMismatch schemaType streamType =
    unless (schemaType == streamType) $
        fail $ "Schema do not match stream: stream/struct type " ++ show streamType ++ ", schema type " ++ show schemaType
