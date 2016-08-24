module Language.Bond.Codegen.Haskell.TypeMapping
    ( haskellTypeMapping
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Data.Text.Lazy.Builder

-- Most of the fields are just use catchers.
-- Can't use mapType for my own type mapping because it returns text, and haskell codegen needs AST type.
haskellTypeMapping :: TypeMapping
haskellTypeMapping = TypeMapping
    Nothing
    (fromString "")
    (singleton '.')
    (error "mapType")
    (error "fixSyntax")
    (error "instanceMapping")
    (error "elementMapping")
    (error "annotatedMapping")
