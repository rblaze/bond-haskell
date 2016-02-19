module Data.Bond.Imports (
    module X,
    IsString,
    Hashable,
    ap,
    makeMap,
    pack
  ) where

import Control.Monad (ap)
import Data.Bond.Proto as X
import Data.Bond.Default as X
import Data.Bond.Types as X
import Data.Bond.Internal.OrdinalSet as X
import Data.Bond.Internal.Protocol as X
import Data.Bond.Internal.TypedSchema as X
import Data.Bond.Internal.Utils as X
import Data.Proxy as X
import Data.Typeable as X
import Data.Hashable (Hashable)
import Data.String
import Data.Text (pack)
import qualified Data.Map as M

makeMap :: Ord k => [(k, v)] -> M.Map k v
makeMap = M.fromList
