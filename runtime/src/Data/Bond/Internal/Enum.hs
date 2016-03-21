module Data.Bond.Internal.Enum where

import Data.Text

-- |Bond enumeration class containing utility functions.
class BondEnum a where
    -- |Convert constant value to name.
    toName :: a -> Maybe Text
    -- |Convert constant name to value.
    fromName :: Text -> Maybe a
