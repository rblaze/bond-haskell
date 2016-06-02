module Data.Bond.Internal.Bonded where

import Control.DeepSeq
import Data.Typeable
import qualified Data.ByteString.Lazy as BL

data Bonded a = BondedStream BL.ByteString | BondedObject a

instance Typeable Bonded
instance Show a => Show (Bonded a)
instance NFData a => NFData (Bonded a)
