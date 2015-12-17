module Data.Bond.ZigZag where

import Data.Int
import Data.Word

class Integral t => ZigZagable t where
    toZigZag :: t -> Word64
    toZigZag i | i >= 0 = 2 * fromIntegral i
    toZigZag i = let long = fromIntegral i :: Int64
                  in (2 * fromIntegral (abs long)) - 1
    fromZigZag :: Word64 -> t
    fromZigZag w | even w = fromIntegral (w `div` 2)
    fromZigZag w = let long = (negate $ fromIntegral ((w - 1) `div` 2) + 1) :: Int64
                    in fromIntegral long

instance ZigZagable Int16
instance ZigZagable Int32
instance ZigZagable Int64
