{-# LANGUAGE FlexibleContexts #-}
module Data.Bond.Internal.OrdinalSet where

import Data.Bond.Types

import qualified Data.IntSet as IS
import qualified Data.Vector.Generic as V

type OrdinalSet = IS.IntSet

deleteOrdinal :: Ordinal -> OrdinalSet -> OrdinalSet
deleteOrdinal (Ordinal d) = IS.delete (fromIntegral d)

memberOrdinal :: Ordinal -> OrdinalSet -> Bool
memberOrdinal (Ordinal d) = IS.member (fromIntegral d)

isEmptySet :: OrdinalSet -> Bool
isEmptySet = IS.null

toOrdinalList :: OrdinalSet -> [Ordinal]
toOrdinalList = map (Ordinal . fromIntegral) . IS.toList

fromOrdinalList :: [Ordinal] -> OrdinalSet
fromOrdinalList = IS.fromList . map (\ (Ordinal d) -> fromIntegral d)

fromOrdinalVector :: (V.Vector v Ordinal, V.Vector v IS.Key) => v Ordinal -> OrdinalSet
fromOrdinalVector = IS.fromList . V.toList . V.map (\ (Ordinal d) -> fromIntegral d)
