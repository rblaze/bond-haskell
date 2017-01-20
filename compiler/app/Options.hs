{-# Language DeriveDataTypeable #-}
module Options where

import Paths_bond_haskell_compiler (version)
import Data.Version (showVersion)
import System.Console.CmdArgs

data Options = Haskell
        { files :: [FilePath]
        , importDir :: [FilePath]
        , outputDir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , hsboot :: Bool
        , hashset :: Bool
        , noShow :: Bool
        , noEq :: Bool
        , generic :: Bool
        , nfData :: Bool
        }
      deriving (Show, Data, Typeable)

haskell :: Options
haskell = Haskell
    { files = def &= typFile &= args
    , importDir = def &= typDir &= name "i" &= help "Add the directory to import search path"
    , outputDir = "." &= typDir &= name "o" &= help "Output generated files into the specified directory"
    , using = def &= typ "MAPPING" &= name "u" &= help "Custom type alias mapping in the form alias=type"
    , namespace = def &= typ "MAPPING" &= name "n" &= help "Custom namespace mapping in the form bond_namespace=language_namespace"
    , hsboot = def &= name "s" &= help "Generate both .hs and .hs-boot files"
    , hashset = def &= name "h" &= help "Use HashSet for set<T> fields"
    , noShow = def &= help "do not derive Show instance for structs"
    , noEq = def &= help "do not derive Eq instance for structs"
    , generic = def &= help "derive Generic instance for structs"
    , nfData = def &= help "derive NFData instance for structs (implies --generic)"
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
