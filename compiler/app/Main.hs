module Main where

import IO
import Options

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(MappingContext))
import Language.Bond.Codegen.Haskell.Decl

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

type Template = MappingContext -> [Declaration] -> CodegenOutput

main :: IO ()
main = do
    args <- getArgs
    options <- (if null args then withArgs ["--help"] else id) getOptions

    let opts = CodegenOpts {
        setType = if hashset options then "HashSet" else "Set",
        deriveEq = not (noEq options),
        deriveShow = not (noShow options)
      }
    let codegens = if hsboot options
        then [decl_hs opts, decl_hsboot opts]
        else [decl_hs opts]
    forM_ (files options) $ codegen options codegens

codegen :: Options -> [Template] -> FilePath -> IO ()
codegen options templates file = do
    (Bond _ namespaces declarations) <- parseFile (import_dir options) file
    let outputDir = output_dir options
    aliasMapping <- parseAliasMappings $ using options
    namespaceMapping <- parseNamespaceMappings $ namespace options
    let mappingContext = MappingContext (error "can't create TypeMapping") aliasMapping namespaceMapping namespaces
    forM_ templates $ \template -> do
        let MultiFile outputFiles = template mappingContext declarations
        forM_ outputFiles $ \(name, code) -> do
            let fileName = outputDir </> name
            createDirectoryIfMissing True $ takeDirectory fileName
            writeFile fileName code