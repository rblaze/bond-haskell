import Distribution.ModuleName (fromString)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks
    { hookedPrograms = [simpleProgram "hbc"]
    , postConf = runHbc
    , buildHook = addSchemaModulesBuild
    , copyHook = addSchemaModulesCopy
    , regHook = addSchemaModulesReg
    , instHook = addSchemaModulesInstall
}

addSchemaModules :: PackageDescription -> LocalBuildInfo -> IO PackageDescription
addSchemaModules pd0 lbi = do
    let outPath = autogenModulesDir lbi
    let schemaFlag = outPath </> "schemagen.flg"
    modules <- lines <$> readFile schemaFlag
    let hbi = (Just $ emptyBuildInfo{ otherModules = map fromString modules }, [])
    return $ updatePackageDescription hbi pd0

addSchemaModulesBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
addSchemaModulesBuild pd0 lbi hooks flags = do
    pd <- addSchemaModules pd0 lbi
    -- run default hook
    buildHook simpleUserHooks pd lbi hooks flags

addSchemaModulesReg :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
addSchemaModulesReg pd0 lbi hooks flags = do
    pd <- addSchemaModules pd0 lbi
    -- run default hook
    regHook simpleUserHooks pd lbi hooks flags

addSchemaModulesCopy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
addSchemaModulesCopy pd0 lbi hooks flags = do
    pd <- addSchemaModules pd0 lbi
    -- run default hook
    copyHook simpleUserHooks pd lbi hooks flags

addSchemaModulesInstall :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
addSchemaModulesInstall pd0 lbi hooks flags = do
    pd <- addSchemaModules pd0 lbi
    -- run default hook
    instHook simpleUserHooks pd lbi hooks flags

runHbc :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
runHbc args conf pd lbi = do
    let verbosity = fromFlagOrDefault normal $ configVerbosity conf
    (hbc, _) <- requireProgram verbosity (simpleProgram "hbc") (configPrograms conf)
    let schemaPath = "schema"
    let schemaFiles =
            [ schemaPath </> "comm.bond"
            , schemaPath </> "transport" </> "packet.bond"
            , schemaPath </> "transport" </> "epoxy_transport.bond"
            ]
    let outPath = autogenModulesDir lbi

    -- generate code for internal bond schemas
    regenSchemas verbosity hbc
        [ "-h"
        , "--hsboot"
        , "--nfdata"
        , "-n", "bond=Data.Bond.Schema"
        , "-i", schemaPath
        ] schemaFiles (buildDir lbi) (outPath </> "schemagen.flg")

    -- generate code for unittests
    when (fromFlagOrDefault False $ configTests conf) $
        regenDirSchemas verbosity hbc ["--nfdata"] ("test" </> "rpc_schemas") outPath (outPath </> "simplegen.flg")

    -- run default hook
    postConf simpleUserHooks args conf pd lbi

regenDirSchemas :: Verbosity -> ConfiguredProgram -> [String] -> FilePath -> FilePath -> FilePath -> IO ()
regenDirSchemas verbosity hbc extraArgs schemasDir outDir flagFile = do
    schemaFiles <- map (schemasDir </>) . filter (".bond" `isSuffixOf`) <$> getDirectoryContents schemasDir
    regenSchemas verbosity hbc extraArgs schemaFiles outDir flagFile

regenSchemas :: Verbosity -> ConfiguredProgram -> [String] -> [FilePath] -> FilePath -> FilePath -> IO ()
regenSchemas verbosity hbc extraArgs schemaFiles outDir flagFile = do
    flagFileExists <- doesFileExist flagFile
    needSchemaRegen <- if flagFileExists
                        then do
                            filesTS <- mapM getModificationTime schemaFiles
                            flagTS <- getModificationTime flagFile
                            return $ any (flagTS <) filesTS
                        else do
                            info verbosity $ "flag file " ++ flagFile ++ " missing"
                            return True
    when needSchemaRegen $ do
        extras <- getProgramOutput verbosity hbc $ ["-o", outDir] ++ extraArgs ++ schemaFiles
        createDirectoryIfMissing True (takeDirectory flagFile)
        writeFile flagFile extras
