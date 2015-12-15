module Language.Bond.Codegen.Haskell.Decl (
        CodegenMode(..),
        CodegenOutput(..),
        decl_hs,
        decl_hsboot
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext)
import Language.Bond.Codegen.Haskell.EnumDecl
import Language.Bond.Codegen.Haskell.StructDecl
import Language.Bond.Codegen.Haskell.Util

import Control.Arrow
import Data.Maybe
import Language.Haskell.Exts
import System.FilePath (joinPath)

data CodegenOutput
    = SingleFile FilePath String
    | MultiFile [(FilePath, String)]

decl_hs :: CodegenMode -> String -> MappingContext -> [Declaration] -> CodegenOutput
decl_hs genMode setType ctx declarations = MultiFile $ mapMaybe step declarations
    where
    step = fmap (second prettyPrint) . makeModule genMode setType ctx

decl_hsboot :: CodegenMode -> String -> MappingContext -> [Declaration] -> CodegenOutput
decl_hsboot genMode setType ctx declarations = MultiFile $ mapMaybe step declarations
    where
    step = fmap (second prettyPrint) . makeHsBootModule genMode setType ctx

makeModule :: CodegenMode -> String -> MappingContext -> Declaration -> Maybe (FilePath, Module)
makeModule genMode setType ctx decl = fmap ((,) sourceName) code
    where
    code = case decl of
        Enum{} -> enumDecl genMode ctx moduleName decl
        Struct{} -> structDecl genMode setType ctx moduleName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    sourceName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs"]
    moduleName = mkModuleName hsNamespaces hsModule

makeHsBootModule :: CodegenMode -> String -> MappingContext -> Declaration -> Maybe (FilePath, Module)
makeHsBootModule genMode setType ctx decl = fmap ((,) hsBootName) code
    where
    code = case decl of
        Enum{} -> enumHsBootDecl genMode ctx moduleName decl
        Struct{} -> structHsBootDecl genMode setType ctx moduleName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    hsBootName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs-boot"]
    moduleName = mkModuleName hsNamespaces hsModule
