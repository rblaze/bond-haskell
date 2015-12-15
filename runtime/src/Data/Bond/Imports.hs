module Data.Bond.Imports (
    module X,
    Hashable,
    ap,
    makeMap
  ) where

import Control.Monad (ap)
--import Data.Bond.Monads as X
import Data.Bond.Proto as X
import Data.Bond.Default as X
import Data.Bond.Types as X
import Data.Bond.Wire as X
import Data.Proxy as X
import Data.Typeable as X
import Data.Hashable (Hashable)
import qualified Data.Map as M

makeMap :: Ord k => [(k, v)] -> M.Map k v
makeMap = M.fromList
