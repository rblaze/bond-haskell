module Language.Bond.Codegen.Haskell.Decl
    ( CodegenOpts(..)
    , CodegenOutput
    , ModuleCode(..)
    , decl_hs
    , decl_hsboot
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext)
import Language.Bond.Codegen.Haskell.AliasDecl
import Language.Bond.Codegen.Haskell.EnumDecl
import Language.Bond.Codegen.Haskell.StructDecl
import Language.Bond.Codegen.Haskell.Util

import Data.Maybe
import Language.Haskell.Exts
import System.FilePath (joinPath)

data ModuleCode = ModuleCode
        { moduleFile :: FilePath
        , moduleName :: Maybe String
        , moduleText :: String
        }

type CodegenOutput = [ModuleCode]

decl_hs :: CodegenOpts -> MappingContext -> [Declaration] -> CodegenOutput
decl_hs opts ctx declarations = mapMaybe step declarations
    where
    step = fmap (\ (f, n, c) -> ModuleCode f n (prettyPrint c)) . makeModule opts ctx

decl_hsboot :: CodegenOpts -> MappingContext -> [Declaration] -> CodegenOutput
decl_hsboot opts ctx declarations = mapMaybe step declarations
    where
    step = fmap (\ (f, n, c) -> ModuleCode f n (prettyPrint c)) . makeHsBootModule opts ctx

makeModule :: CodegenOpts -> MappingContext -> Declaration -> Maybe (FilePath, Maybe String, Module)
makeModule opts ctx decl = fmap (\ c -> (sourceName, Just printName, c)) code
    where
    code = case decl of
        Enum{} -> enumDecl opts ctx modName decl
        Struct{} -> structDecl opts ctx modName decl
        Alias{} -> aliasDecl opts ctx modName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    sourceName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs"]
    modName = mkModuleName hsNamespaces hsModule
    ModuleName printName = modName

makeHsBootModule :: CodegenOpts -> MappingContext -> Declaration -> Maybe (FilePath, Maybe String, Module)
makeHsBootModule opts ctx decl = fmap (\ c -> (hsBootName, Nothing, c)) code
    where
    code = case decl of
        Enum{} -> enumHsBootDecl opts ctx modName decl
        Struct{} -> structHsBootDecl opts ctx modName decl
        _ -> Nothing
    hsModule = capitalize (makeDeclName decl)
    hsNamespaces = map capitalize $ getNamespace ctx
    hsBootName = joinPath $ hsNamespaces ++ [hsModule ++ ".hs-boot"]
    modName = mkModuleName hsNamespaces hsModule
