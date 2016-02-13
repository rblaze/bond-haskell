module Data.Bond.Bonded where

import Data.Typeable
import qualified Data.ByteString.Lazy as BL

data Bonded a = BondedStream BL.ByteString | BondedObject a

instance Typeable Bonded
instance Show a => Show (Bonded a)
