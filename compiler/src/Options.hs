{-# Language DeriveDataTypeable #-}
module Options where

import Paths_bond_haskell_compiler (version)
import Data.Version (showVersion)
import System.Console.CmdArgs

data Options = Haskell
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , schema_bootstrap :: Bool
        , hashset :: Bool
        }
      deriving (Show, Data)

haskell :: Options
haskell = Haskell
    { files = def &= typFile &= args
    , import_dir = def &= typDir &= name "i" &= help "Add the directory to import search path"
    , output_dir = "." &= typDir &= name "o" &= help "Output generated files into the specified directory"
    , using = def &= typ "MAPPING" &= name "u" &= help "Custom type alias mapping in the form alias=type"
    , namespace = def &= typ "MAPPING" &= name "n" &= help "Custom namespace mapping in the form bond_namespace=language_namespace"
    , schema_bootstrap = def &= name "s" &= help "Generate special code for runtime schema structures (internal use)"
    , hashset = def &= name "h" &= help "Use HashSet for set<T> fields"
    } &=
    name "haskell" &=
    help "Generate Haskell code"

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [haskell &= auto] &=
    program "hbc" &=
    help "Compile Bond schema file(s) and generate specified output. The schema file(s) can be in one of two formats: Bond IDL or JSON representation of the schema abstract syntax tree as produced by `gbc schema`. Multiple schema files can be specified either directly on the command line or by listing them in a text file passed to gbc via @listfile syntax." &=
    summary ("Bond2Haskell Compiler " ++ showVersion version)

getOptions :: IO Options
getOptions = cmdArgsRun mode
