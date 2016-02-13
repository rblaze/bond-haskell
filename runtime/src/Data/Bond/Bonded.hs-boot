module Data.Bond.Bonded where

import qualified Data.ByteString.Lazy as BL

data Bonded a = BondedStream BL.ByteString | BondedObject a

instance Show a => Show (Bonded a)
