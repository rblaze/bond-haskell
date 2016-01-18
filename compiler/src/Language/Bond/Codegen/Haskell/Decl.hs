module Language.Bond.Codegen.Haskell.Decl (
        CodegenOpts(..),
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

decl_hs :: CodegenOpts -> MappingContext -> [Declaration] -> CodegenOutput
decl_hs opts ctx declarations = MultiFile $ mapMaybe step declarations
    where
    step = fmap (second prettyPrint) . makeModule opts ctx

decl_hsboot :: CodegenOpts -> MappingContext -> [Declaration] -> CodegenOutput
decl_hsboot opts ctx declarations = MultiFile $ mapMaybe step declarations
    where
    step = fmap (second prettyPrint) . makeHsBootModule opts ctx

makeModule :: CodegenOpts -> MappingContext -> Declaration -> Maybe (FilePath, Module)
makeModule opts ctx decl = fmap ((,) sourceName) code
    where
    code = case decl of
        Enum{} -> enumDecl opts ctx moduleName decl
        Struct{} -> structDecl opts ctx moduleName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    sourceName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs"]
    moduleName = mkModuleName hsNamespaces hsModule

makeHsBootModule :: CodegenOpts -> MappingContext -> Declaration -> Maybe (FilePath, Module)
makeHsBootModule opts ctx decl = fmap ((,) hsBootName) code
    where
    code = case decl of
        Enum{} -> enumHsBootDecl opts ctx moduleName decl
        Struct{} -> structHsBootDecl opts ctx moduleName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    hsBootName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs-boot"]
    moduleName = mkModuleName hsNamespaces hsModule
