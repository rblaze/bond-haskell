{-# Language ScopedTypeVariables #-}
import Data.Bond
import Data.Bond.Marshal
import Data.Bond.ZigZag
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.SchemaDef
import Unittest.Compat.Another.Another
import Unittest.Compat.BasicTypes
import Unittest.Compat.Compat

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either hiding (left, right)
import Data.Int
import Data.Proxy
import Data.Word
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import DataPath

main :: IO ()
main = defaultMain tests

compatDataPath :: String
compatDataPath = "test" </> "compat" </> "data"

brokenSchemasPath :: String
brokenSchemasPath = "test" </> "broken_schemas"

simpleSchemasPath :: String
simpleSchemasPath = "test" </> "simple_schemas"

tests :: TestTree
tests = testGroup "Haskell runtime tests"
    [ testGroup "Runtime schema tests"
        [ testCase "check saved Compat schema matching our schema" matchCompatSchemaDef,
          testCase "check gbc-generated SchemaDef schema matching our schema" matchGeneratedSchemaDef
        ],
      testGroup "Runtime schema validation tests"
        [ testCaseInfo "loop in inheritance chain" $ checkSchemaError "inheritance_loop.json",
          testCaseInfo "non-struct in inheritance chain" $ checkSchemaError "inherit_from_int.json",
          testCaseInfo "index out of range" $ checkSchemaError "index_out_of_range.json",
          testCaseInfo "field type mismatch" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 10, BOOL True)],
          testCaseInfo "field type mismatch in field struct" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList
              [
                (Ordinal 20, STRUCT $ Struct Nothing $ M.fromList [(Ordinal 10, BOOL True)])
              ],
          testCaseInfo "type mismatch in list" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 40, LIST bT_BOOL [])],
          testCaseInfo "type mismatch in list element struct" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 40, LIST bT_STRUCT
              [
                STRUCT $ Struct (Just $ Struct Nothing M.empty) $ M.fromList [(Ordinal 10, BOOL True)]
              ])],
          testCaseInfo "type mismatch in set" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 50, SET bT_BOOL [])],
          testCaseInfo "type mismatch in map key" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_BOOL bT_STRUCT [])],
          testCaseInfo "type mismatch in map value" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_BOOL [])],
          testCaseInfo "type mismatch in map element key" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
              [
                (BOOL True, STRUCT $ Struct (Just $ Struct Nothing M.empty) M.empty)
              ])],
          testCaseInfo "type mismatch in map element value" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
              [
                (UINT32 42, BOOL True)
              ])],
          testCaseInfo "type mismatch in map element field" $
            checkSchemaMismatch "test.outer.json" $ Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
              [
                (UINT32 42, STRUCT $ Struct (Just $ Struct Nothing M.empty) $ M.fromList [(Ordinal 10, BOOL True)])
              ])],
          testCaseInfo "schema too deep for struct" $
            checkSchemaMismatch "test.inner.json" $ Struct Nothing M.empty,
          testCase "shallow schema" checkShallowSchema
        ],
      testGroup "Protocol tests"
        [ testGroup "SimpleBinary"
            [ testCase "read/write Compat value" $
                readCompat SimpleBinaryProto "compat.simple2.dat",
              testCase "read Compat with compile-time schema" $
                readCompatWithSchema SimpleBinaryProto "compat.simple2.dat",
              testCase "read Compat with runtime schema" $
                readCompatWithRuntimeSchema SimpleBinaryProto "compat.simple2.dat"
            ],
          testGroup "SimpleBinary v1"
            [ testCase "read/write Compat value" $
                readCompat SimpleBinaryV1Proto "compat.simple.dat",
              testCase "read Compat with compile-time schema" $
                readCompatWithSchema SimpleBinaryV1Proto "compat.simple.dat",
              testCase "read Compat with runtime schema" $
                readCompatWithRuntimeSchema SimpleBinaryV1Proto "compat.simple.dat"
            ],
          testGroup "FastBinary"
            [ testCase "read/write Compat value" $
                readCompat FastBinaryProto "compat.fast.dat",
              testCase "read BasicTypes value" $
                readAsType FastBinaryProto (Proxy :: Proxy BasicTypes) "compat.fast.dat",
              testCase "read Another value" $
                readAsType FastBinaryProto (Proxy :: Proxy Another) "compat.fast.dat",
              testCase "read Compat w/o schema" $
                readCompatTagged FastBinaryProto "compat.fast.dat",
              testCase "read Compat with compile-time schema" $
                readCompatWithSchema FastBinaryProto "compat.fast.dat",
              testCase "read Compat with runtime schema" $
                readCompatWithRuntimeSchema FastBinaryProto "compat.fast.dat",
              invalidTaggedWriteTests FastBinaryProto
            ],
          testGroup "CompactBinary"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryProto "compat.compact2.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryProto (Proxy :: Proxy BasicTypes) "compat.compact2.dat",
              testCase "read Another value" $
                readAsType CompactBinaryProto (Proxy :: Proxy Another) "compat.compact2.dat",
              testCase "read Compat w/o schema" $
                readCompatTagged CompactBinaryProto "compat.compact2.dat",
              testCase "read Compat with compile-time schema" $
                readCompatWithSchema CompactBinaryProto "compat.compact2.dat",
              testCase "read Compat with runtime schema" $
                readCompatWithRuntimeSchema CompactBinaryProto "compat.compact2.dat",
              invalidTaggedWriteTests CompactBinaryProto
            ],
          testGroup "CompactBinary v1"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryV1Proto "compat.compact.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy BasicTypes) "compat.compact.dat",
              testCase "read Another value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy Another) "compat.compact.dat",
              testCase "read Compat w/o schema" $
                readCompatTagged CompactBinaryV1Proto "compat.compact.dat",
              testCase "read Compat with compile-time schema" $
                readCompatWithSchema CompactBinaryV1Proto "compat.compact.dat",
              testCase "read Compat with runtime schema" $
                readCompatWithRuntimeSchema CompactBinaryV1Proto "compat.compact.dat",
              invalidTaggedWriteTests CompactBinaryV1Proto
            ],
          testGroup "JSON"
            [ testJson "read/write original Compat value" "compat.json.dat",
              testJson "read/write golden Compat value" "golden.json.dat",
              testCase "read BasicTypes value" $
                readAsType JsonProto (Proxy :: Proxy BasicTypes) "compat.json.dat",
              testCase "read Another value" $
                readAsType JsonProto (Proxy :: Proxy Another) "compat.json.dat"
            ],
          testGroup "Marshalling"
            [ testCase "read/write SchemaDef value" readSchema,
              testCase "read/write SchemaDef value with schema" readSchemaWithSchema,
              testCase "read/write SchemaDef value w/o schema" readSchemaTagged
            ],
          testGroup "Cross-tests" crossTests
        ],
      testGroup "ZigZag encoding"
        [ testProperty "Int16" zigzagInt16,
          testProperty "Int32" zigzagInt32,
          testProperty "Int64" zigzagInt64,
          testProperty "Word64" zigzagWord64
        ]
    ]

invalidTaggedWriteTests :: BondTaggedProto t => t -> TestTree
invalidTaggedWriteTests t = testGroup "invalid tagged write tests"
  [
    testCaseInfo "type mismatch in list" $
        testInvalidTaggedWrite t $ Struct Nothing $ M.fromList [(Ordinal 1, LIST bT_BOOL [INT16 42, BOOL True])],
    testCaseInfo "type mismatch inside list element" $
        testInvalidTaggedWrite t $ Struct Nothing
            $ M.fromList [(Ordinal 1, LIST bT_SET [SET bT_INT32 [INT32 5, UINT32 6]])],
    testCaseInfo "type mismatch in set" $
        testInvalidTaggedWrite t $ Struct Nothing
            $ M.fromList [(Ordinal 1, SET bT_STRUCT [STRUCT $ Struct Nothing M.empty, BOOL True])],
    testCaseInfo "type mismatch in map key" $
        testInvalidTaggedWrite t $ Struct Nothing
            $ M.fromList [(Ordinal 1, MAP bT_UINT32 bT_STRING [(UINT64 1, STRING $ fromString "bar")])],
    testCaseInfo "type mismatch in map value" $
        testInvalidTaggedWrite t $ Struct Nothing
            $ M.fromList [(Ordinal 1, MAP bT_UINT64 bT_WSTRING [(UINT64 1, STRING $ fromString "foo")])]
  ]

crossTests :: [TestTree]
crossTests =
    [crossTest "" left right | left <- simpleProtos, right <- simpleProtos, getName left < getName right] ++
    [crossTest "" left right | left <- protos, right <- protos, getName left < getName right] ++
    [crossTest "w/o schema: " left right | left <- taggedProtos, right <- taggedProtos, getName left < getName right]
    where
    -- Simple protocol has different m_defaults from all others. Also some enum values differ.
    -- Json differs in uint64 values.
    -- See comments in https://github.com/Microsoft/bond/blob/master/cpp/test/compat/serialization.cpp
    simpleProtos = [
        ("Simple", bondRead SimpleBinaryProto :: L.ByteString -> Either String Compat, "compat.simple2.dat"),
        ("Simple v1", bondRead SimpleBinaryV1Proto, "compat.simple.dat")
     ]
    protos = [
        ("Compact", bondRead CompactBinaryProto :: L.ByteString -> Either String Compat, "compat.compact2.dat"),
        ("Compact v1", bondRead CompactBinaryV1Proto, "compat.compact.dat"),
        ("Fast", bondRead FastBinaryProto, "compat.fast.dat")
     ]
    taggedProtos = [
        ("Compact", bondReadTagged CompactBinaryProto :: L.ByteString -> Either String Struct, "compat.compact2.dat"),
        ("Compact v1", bondReadTagged CompactBinaryV1Proto, "compat.compact.dat"),
        ("Fast", bondReadTagged FastBinaryProto, "compat.fast.dat")
     ]
    getName (n, _, _) = n
    crossTest prefix (lname, lreader, lfile) (rname, rreader, rfile)
        = testCase (prefix ++ lname ++ " - " ++ rname) $ do
            ldat <- L.readFile (compatDataPath </> lfile)
            let lparse = lreader ldat
            whenLeft lparse assertFailure
            rdat <- L.readFile (compatDataPath </> rfile)
            let rparse = rreader rdat
            whenLeft rparse assertFailure
            assertEqual "values do not match" lparse rparse

readCompat :: BondProto t => t -> String -> Assertion
readCompat p f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    s <- hoistEither (bondRead p dat :: Either String Compat)
    let d' = bondWrite p s
    checkEqual "serialized value do not match original" dat d'

readCompatTagged :: BondTaggedProto t => t -> String -> Assertion
readCompatTagged p f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    s <- hoistEither $ bondReadTagged p dat
    outdat <- hoistEither $ bondWriteTagged p s
    checkEqual "serialized value do not match original" dat outdat

readCompatWithSchema :: BondProto t => t -> String -> Assertion
readCompatWithSchema p f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    let schema = getSchema (Proxy :: Proxy Compat)
    s <- hoistEither $ bondReadWithSchema p schema dat
    outdat <- hoistEither $ bondWriteWithSchema p schema s
    checkEqual "serialized value do not match original" dat outdat

readCompatWithRuntimeSchema :: BondProto t => t -> String -> Assertion
readCompatWithRuntimeSchema p f = assertEither $ do
    schemadat <- readData (compatDataPath </> "compat.schema.dat")
    schema <- hoistEither (bondUnmarshal schemadat :: Either String SchemaDef)
    dat <- readData (compatDataPath </> f)
    s <- hoistEither $ bondReadWithSchema p schema dat
    outdat <- hoistEither $ bondWriteWithSchema p schema s
    checkEqual "serialized value do not match original" dat outdat

readSchema :: Assertion
readSchema = assertEither $ do
    dat <- readData (compatDataPath </> "compat.schema.dat")
    s <- hoistEither (bondUnmarshal dat :: Either String SchemaDef)
    let d' = bondMarshal CompactBinaryV1Proto s
    checkEqual "serialized value do not match original" dat d'

readSchemaWithSchema :: Assertion
readSchemaWithSchema = assertEither $ do
    let schema = getSchema (Proxy :: Proxy SchemaDef)
    dat <- readData (compatDataPath </> "compat.schema.dat")
    struct <- hoistEither $ bondUnmarshalWithSchema schema dat
    out <- hoistEither $ bondMarshalWithSchema CompactBinaryV1Proto schema struct
    checkEqual "serialized value do not match original" dat out

readSchemaTagged :: Assertion
readSchemaTagged = assertEither $ do
    dat <- readData (compatDataPath </> "compat.schema.dat")
    struct <- hoistEither $ bondUnmarshalTagged dat
    let schema = getSchema (Proxy :: Proxy SchemaDef)
    checked <- hoistEither $ checkStructSchema schema struct
    out <- hoistEither $ bondMarshalTagged CompactBinaryV1Proto checked
    checkEqual "serialized value do not match original" dat out

-- compat.json.dat file has several properties making it incompatible with usual test:
-- all float values saved with extra precision
-- double values are saved in a way different from one used by scientific
-- fields are saved in declaration order, while aeson saves them in pseudorandom hash order
-- optional fields with default values are present

-- Golden file is manually checked to match compat data as best as it could

testJson :: String -> FilePath -> TestTree
testJson name f = goldenVsString name (compatDataPath </> "golden.json.dat") $ do
    dat <- L.readFile (compatDataPath </> f)
    let parse = bondRead JsonProto dat :: Either String Compat
    case parse of
        Left msg -> do
            assertFailure msg
            return L.empty
        Right s -> do
                    let d' = bondWrite JsonProto s
                    return d'

readAsType :: forall t a. (Show a, BondProto t, BondStruct a) => t -> Proxy a -> String -> Assertion
readAsType p _ f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    void $ hoistEither (bondRead p dat :: Either String a)

matchCompatSchemaDef :: Assertion
matchCompatSchemaDef = assertEither $ do
    dat <- readData (compatDataPath </> "compat.schema.dat")
    s <- hoistEither $ bondUnmarshal dat
    let s' = getSchema (Proxy :: Proxy Compat)
    checkEqual "schemas do not match" s s'

-- gbc's json schemas differ slightly from bond.bond definition,
-- so tests can't compare json representations.
matchGeneratedSchemaDef :: Assertion
matchGeneratedSchemaDef = assertEither $ do
    dat <- readData (autogenPath </> "bond.SchemaDef.json")
    s <- hoistEither $ bondRead JsonProto dat
    let s' = getSchema (Proxy :: Proxy SchemaDef)
    checkEqual "schemas do not match" s s'

checkSchemaError :: FilePath -> IO String
checkSchemaError f = assertWithMsg $ do
    dat <- readData (brokenSchemasPath </> f)
    schema <- hoistEither $ bondRead JsonProto dat
    checkHasError $ checkStructSchema schema (Struct Nothing M.empty)

checkSchemaMismatch :: FilePath -> Struct -> IO String
checkSchemaMismatch f s = assertWithMsg $ do
    dat <- readData (simpleSchemasPath </> f)
    schema <- hoistEither $ bondRead JsonProto dat
    checkHasError $ checkStructSchema schema s

checkShallowSchema :: Assertion
checkShallowSchema = assertEither $ do
    dat <- readData (simpleSchemasPath </> "test.outer.json")
    schema <- hoistEither $ bondRead JsonProto dat
    void $ hoistEither $ checkStructSchema schema $ Struct (Just $ Struct Nothing M.empty) M.empty

testInvalidTaggedWrite :: BondTaggedProto t => t -> Struct -> IO String
testInvalidTaggedWrite p s
    = case bondWriteTagged p s of
        Left msg -> return $ "error caught: " ++ msg
        Right _ -> assertFailure "error not caught" >> return ""

zigzagInt16 :: Int16 -> Bool
zigzagInt16 x = x == fromZigZag (toZigZag x)

zigzagInt32 :: Int32 -> Bool
zigzagInt32 x = x == fromZigZag (toZigZag x)

zigzagInt64 :: Int64 -> Bool
zigzagInt64 x = x == fromZigZag (toZigZag x)

zigzagWord64 :: Word64 -> Bool
zigzagWord64 x = x == toZigZag (fromZigZag x :: Int64)

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Right _) _ = return ()
whenLeft (Left a) f = f a

assertEither :: EitherT String IO () -> Assertion
assertEither = eitherT assertFailure (const $ return ())

assertWithMsg :: EitherT String IO String -> IO String
assertWithMsg = eitherT (\ msg -> assertFailure msg >> return "") (\ msg -> return $ "error caught: " ++ msg)

readData :: FilePath -> EitherT String IO L.ByteString
readData = lift . L.readFile

checkEqual :: (Eq a, Show a, MonadTrans t) => String -> a -> a -> t IO ()
checkEqual m a b = lift $ assertEqual m a b

checkHasError :: Either String a -> EitherT String IO String
checkHasError = bimapEitherT (const "error not found") id . swapEitherT . hoistEither
