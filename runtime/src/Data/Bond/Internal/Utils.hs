module Data.Bond.Internal.Utils where

import qualified Data.Map as M
import qualified Data.Text as T

makeGenericName :: T.Text -> [T.Text] -> T.Text
makeGenericName upper xs = upper `T.append` T.singleton '<'
                            `T.append` T.intercalate (T.singleton '.') xs
                            `T.append` T.singleton '>'

makeMap :: Ord k => [(k, v)] -> M.Map k v
makeMap = M.fromList
