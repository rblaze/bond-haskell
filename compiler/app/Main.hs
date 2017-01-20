module Main where

import IO
import Options

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Haskell.TypeMapping
import Language.Bond.Codegen.Haskell.Decl

import Control.Monad
import Data.Traversable
import System.Directory
import System.Environment
import System.FilePath
import Prelude -- workaround for Data.Traversable in ghc 7.10

type Template = MappingContext -> [Declaration] -> CodegenOutput

main :: IO ()
main = do
    args <- getArgs
    options <- (if null args then withArgs ["--help"] else id) getOptions

    let opts = CodegenOpts
            { setType = if hashset options then "HashSet" else "Set"
            , deriveEq = not (noEq options)
            , deriveShow = not (noShow options)
            , deriveNFData = nfData options
            , deriveGeneric = nfData options || generic options
            }
    let codegens = if hsboot options
        then [declHs opts, declHsBoot opts]
        else [declHs opts]
    forM_ (files options) $ codegen options codegens

codegen :: Options -> [Template] -> FilePath -> IO ()
codegen options templates file = do
    (Bond _ fileNamespaces declarations) <- parseFile (importDir options) file
    fileAliasMapping <- parseAliasMappings $ using options
    fileNamespaceMapping <- parseNamespaceMappings $ namespace options
    let mappingContext = MappingContext haskellTypeMapping fileAliasMapping fileNamespaceMapping fileNamespaces
    forM_ templates $ \template -> do
        let outputFiles = template mappingContext declarations
        forM_ outputFiles $ \ moduleInfo -> do
            let fileName = outputDir options </> moduleFile moduleInfo
            createDirectoryIfMissing True $ takeDirectory fileName
            writeFile fileName (moduleText moduleInfo)
            traverse putStrLn (moduleName moduleInfo)
