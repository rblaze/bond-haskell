module Language.Bond.Codegen.Haskell.TypeMapping
    ( haskellTypeMapping
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Data.Text.Lazy.Builder

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
