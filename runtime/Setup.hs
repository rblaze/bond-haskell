import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Verbosity
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks {
    hookedPrograms = [simpleProgram "hbc", simpleProgram "gbc"],
    postConf = runHbc
}

runHbc :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
runHbc args conf pd lbi = do
    let verbosity = fromFlagOrDefault normal $ configVerbosity conf
    (hbc, _) <- requireProgram verbosity (simpleProgram "hbc") (configPrograms conf)
    (gbc, _) <- requireProgram verbosity (simpleProgram "gbc") (configPrograms conf)
    let schemaPath = "schema"
    let outPath = autogenModulesDir lbi

    -- generate code for SchemaDef & Co
    let schemaFlag = outPath </> "schemagen.flg"
    schemaGenFlagExists <- doesFileExist schemaFlag
    needSchemaRegen <- if schemaGenFlagExists
                        then do
                            bondTS <- getModificationTime $ schemaPath </> "bond.bond"
                            bondConstTS <- getModificationTime $ schemaPath </> "bond_const.bond"
                            flagTS <- getModificationTime schemaFlag
                            return (flagTS < bondTS || flagTS < bondConstTS)
                        else return True
    when needSchemaRegen $ do
        runProgram verbosity hbc ["-h", "-o", buildDir lbi, "--schema-bootstrap", "-n", "bond=Data.Bond.Schema", schemaPath </> "bond.bond", schemaPath </> "bond_const.bond"]
        createDirectoryIfMissing False outPath
        writeFile schemaFlag ""

    -- generate json schemas for unittests
    runProgram verbosity gbc ["schema", "-o", outPath, "-r", schemaPath </> "bond.bond"]
    writeFile (outPath </> "DataPath.hs") $
        "module DataPath where\n\
        \autogenPath :: String\n\
        \autogenPath = " ++ show outPath

    -- generate code for unittests
    let testSchemasPath = "test" </> "compat" </> "schemas"
    let compatFlag = outPath </> "compatgen.flg"
    testSchemaFiles <- map (testSchemasPath </>) . filter (".bond" `isSuffixOf`) <$>
                getDirectoryContents testSchemasPath
    compatGenFlagExists <- doesFileExist compatFlag
    needCompatRegen <- if compatGenFlagExists
                        then do
                            filesTS <- mapM getModificationTime testSchemaFiles
                            flagTS <- getModificationTime compatFlag
                            return $ any (flagTS <) filesTS
                        else return True
    when (needCompatRegen && (fromFlagOrDefault False $ configTests conf)) $ do
        runProgram verbosity hbc $ ["-o", outPath] ++ testSchemaFiles
        writeFile compatFlag ""

    -- run default hook
    postConf simpleUserHooks args conf pd lbi
