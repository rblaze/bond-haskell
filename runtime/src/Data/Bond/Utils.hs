module Data.Bond.Utils where

import Data.Bond.Monads

import Control.Applicative
import Data.Bits
import Prelude          -- ghc 7.10 workaround for Control.Applicative

getVarInt :: BondGet t Int
getVarInt = step (0 :: Int)
    where
--    step :: Int -> BondGetM Int
    step n | n > 4 = fail "VarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Int)
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

putVarInt :: (Bits a, Integral a) => a -> BondPut t
putVarInt i | i < 0 = error "VarInt with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = let iLow = fromIntegral $ i .&. 0x7F
               in do
                    putWord8 $ iLow `setBit` 7
                    putVarInt (i `shiftR` 7)
