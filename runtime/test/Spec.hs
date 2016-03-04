{-# Language GADTs, ScopedTypeVariables, OverloadedStrings #-}
import Data.Bond
import Data.Bond.Internal.ZigZag
import Data.Bond.Schema.BondDataType
import Data.Bond.Schema.SchemaDef
import Unittest.Compat.Another.Another
import Unittest.Compat.BasicTypes
import Unittest.Compat.Compat
import Unittest.Simple.Inner as Inner
import Unittest.Simple.Outer as Outer
import Unittest.Simple.Reqopt as ReqOpt

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as M

import DataPath

main :: IO ()
main = defaultMain tests

data ProtoWrapper = forall t. BondProto t => ProtoWrapper String t FilePath
                  | forall t. BondTaggedProto t => TaggedProtoWrapper String t FilePath

fromTagged :: ProtoWrapper -> ProtoWrapper
fromTagged (TaggedProtoWrapper n t dat) = ProtoWrapper n t dat
fromTagged w = w

taggedProtocols :: [ProtoWrapper]
taggedProtocols =
    [ TaggedProtoWrapper "CompactBinary" CompactBinaryProto "compat.compact2.dat"
    , TaggedProtoWrapper "CompactBinary v1" CompactBinaryV1Proto "compat.compact.dat"
    , TaggedProtoWrapper "FastBinary" FastBinaryProto "compat.fast.dat"
    ]

simpleProtocols :: [ProtoWrapper]
simpleProtocols =
    [ ProtoWrapper "SimpleBinary" SimpleBinaryProto "compat.simple2.dat"
    , ProtoWrapper "SimpleBinary v1" SimpleBinaryV1Proto "compat.simple.dat"
    ]

jsonProtocol :: ProtoWrapper
jsonProtocol = ProtoWrapper "JSON" JsonProto "compat.json.dat"

untaggedProtocols :: [ProtoWrapper]
untaggedProtocols = jsonProtocol : simpleProtocols

allProtocols :: [ProtoWrapper]
allProtocols = untaggedProtocols ++ taggedProtocols

compatDataPath :: String
compatDataPath = "test" </> "compat" </> "data"

brokenSchemasPath :: String
brokenSchemasPath = "test" </> "broken_schemas"

tests :: TestTree
tests = testGroup "Haskell runtime tests"
    [ testGroup "Runtime schema tests"
        [ testCase "check saved Compat schema matching our schema" matchCompatSchemaDef,
          testCase "check gbc-generated SchemaDef schema matching our schema" matchGeneratedSchemaDef,
          testCase "check compile/decompile schema is identity" checkCompileIdentity,
          testCase "defaultStruct is correct" checkDefaultStruct
        ],
      testGroup "Runtime schema validation tests"
        [ testCaseInfo "loop in inheritance chain" $ checkSchemaError "inheritance_loop.json",
          testCaseInfo "non-struct in inheritance chain" $ checkSchemaError "inherit_from_int.json",
          testCaseInfo "index out of range" $ checkSchemaError "index_out_of_range.json",
          testCaseInfo "field type mismatch" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $ Struct Nothing $ M.fromList [(Ordinal 10, BOOL True)],
          testCaseInfo "field type mismatch in field struct" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $ Struct Nothing $ M.fromList
              [
                (Ordinal 20, STRUCT $ Struct Nothing $ M.fromList [(Ordinal 10, BOOL True)])
              ],
          testCaseInfo "type mismatch in list" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 40, LIST bT_BOOL [])],
          testCaseInfo "type mismatch in list element struct" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 40, LIST bT_STRUCT
                  [
                    STRUCT $ Struct (Just $ Struct Nothing M.empty) $ M.fromList [(Ordinal 10, BOOL True)]
                  ])],
          testCaseInfo "type mismatch in set" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 50, SET bT_BOOL [])],
          testCaseInfo "type mismatch in map key" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_BOOL bT_STRUCT [])],
          testCaseInfo "type mismatch in map value" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_BOOL [])],
          testCaseInfo "type mismatch in map element key" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
                  [
                    (BOOL True, STRUCT $ Struct (Just $ Struct Nothing M.empty) M.empty)
                  ])],
          testCaseInfo "type mismatch in map element value" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
                  [
                    (UINT32 42, BOOL True)
                  ])],
          testCaseInfo "type mismatch in map element field" $
            checkSchemaMismatch (Proxy :: Proxy Outer) $
                Struct Nothing $ M.fromList [(Ordinal 60, MAP bT_UINT32 bT_STRUCT
                  [
                    (UINT32 42, STRUCT $ Struct (Just $ Struct Nothing M.empty) $ M.fromList [(Ordinal 10, BOOL True)])
                  ])],
          testCaseInfo "schema too deep for struct" $
            checkSchemaMismatch (Proxy :: Proxy Inner) $ Struct Nothing M.empty,
          testCase "shallow schema" checkShallowSchema
        ],
      testGroup "Protocol tests" $
        (map testTagged taggedProtocols) ++
        (map testSimple simpleProtocols) ++
          [ testGroup "JSON"
            [ testJson "read/write original Compat value" "compat.json.dat",
              testJson "read/write golden Compat value" "golden.json.dat",
--              testJsonWithSchema "read/write original Compat value with schema" "compat.json.dat",
              testJsonWithSchema "read/write golden Compat value with schema" "golden.json.dat",
--              testJsonWithRuntimeSchema "read/write original Compat value with runtime schema" "compat.json.dat",
              testJsonWithRuntimeSchema "read/write golden Compat value with runtime schema" "golden.json.dat",
              testCase "read BasicTypes value" $
                readAsType JsonProto (Proxy :: Proxy BasicTypes) "compat.json.dat",
              testCase "read Another value" $
                readAsType JsonProto (Proxy :: Proxy Another) "compat.json.dat",
              testCaseInfo "fail to read with required field missing" $ jsonFailToReadMissingRequired,
              testCaseInfo "fail to write nothing to required field" $ failToWriteRequiredNothing JsonProto
            ],
          testGroup "Marshalling"
            [ testCase "read/write SchemaDef value" readSchema,
              testCase "read/write SchemaDef value with schema" readSchemaWithSchema,
              testCase "read/write SchemaDef value w/o schema" readSchemaTagged
            ],
          testGroup "Cross-tests" crossTests,
          testGroup "Bonded recoding" bondedRecodeTests
        ],
      testGroup "ZigZag encoding"
        [ testProperty "Int16" zigzagInt16,
          testProperty "Int32" zigzagInt32,
          testProperty "Int64" zigzagInt64,
          testProperty "Word64" zigzagWord64
        ]
    ]
    where
    testTagged (TaggedProtoWrapper name proto dat) = testGroup name
        [ testCase "read/write Compat value" $ readCompat proto dat
        , testCase "read BasicTypes value" $ readAsType proto (Proxy :: Proxy BasicTypes) dat
        , testCase "read Another value" $ readAsType proto (Proxy :: Proxy Another) dat
        , testCase "read Compat w/o schema" $ readCompatTagged proto dat
        , testCase "read Compat with compile-time schema" $ readCompatWithSchema proto dat
        , testCase "read Compat with runtime schema" $ readCompatWithRuntimeSchema proto dat
        , testCaseInfo "fail to read with required field missing" $ failToReadMissingRequired proto
        , testCaseInfo "fail to read with schema and required field missing" $ failToReadMissingRequiredWithSchema proto
        , testCaseInfo "fail to write nothing to required field" $ failToWriteRequiredNothing proto
        , testCaseInfo "fail to write nothing to required field with schema" $ failToWriteRequiredNothingWithSchema proto
        , invalidTaggedWriteTests proto
        ]
    testTagged _ = error "invalid protocol"

    testSimple (ProtoWrapper name proto dat) = testGroup name
        [ testCase "read/write Compat value" $ readCompat proto dat
        , testCase "read Compat with compile-time schema" $ readCompatWithSchema proto dat
        , testCase "read Compat with runtime schema" $ readCompatWithRuntimeSchema proto dat
        , testCaseInfo "fail to save default nothing" $ failToSaveDefaultNothing proto
        ]
    testSimple _ = error "invalid protocol"

bondedRecodeTests :: [TestTree]
bondedRecodeTests = [testCase (name1 ++ " - " ++ name2) (recodeFromTo t1 t2)
                        | ProtoWrapper name1 t1 _ <- map fromTagged allProtocols
                        , ProtoWrapper name2 t2 _ <- map fromTagged allProtocols
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
            $ M.fromList [(Ordinal 1, MAP bT_UINT32 bT_STRING [(UINT64 1, STRING "bar")])],
    testCaseInfo "type mismatch in map value" $
        testInvalidTaggedWrite t $ Struct Nothing
            $ M.fromList [(Ordinal 1, MAP bT_UINT64 bT_WSTRING [(UINT64 1, STRING "foo")])]
  ]

-- Simple protocol has different m_defaults from all others. Also some enum values differ.
-- Json differs in uint64 values.
-- See comments in https://github.com/Microsoft/bond/blob/master/cpp/test/compat/serialization.cpp
crossTests :: [TestTree]
crossTests =
    [crossTest left right
        | left <- simpleProtocols
        , right <- simpleProtocols
        , getName left < getName right
    ] ++
    [crossTest left right
        | left <- taggedProtocols
        , right <- taggedProtocols
        , getName left < getName right
    ] ++
    [crossTestTagged left right
        | left <- taggedProtocols
        , right <- taggedProtocols
        , getName left < getName right
    ]
    where
    getName (ProtoWrapper name _ _) = name
    getName (TaggedProtoWrapper name _ _) = name

    crossTest (ProtoWrapper lname lproto lfile) (ProtoWrapper rname rproto rfile)
        = testCase (lname ++ " - " ++ rname) $ assertEither $ do
            ldata <- readData (compatDataPath </> lfile)
            left <- hoistEither (bondRead lproto ldata :: Either String Compat)
            rdata <- readData (compatDataPath </> rfile)
            right <- hoistEither $ bondRead rproto rdata
            checkEqual "values do not match" left right
            lout <- hoistEither $ bondWrite lproto right
            checkEqual ("value saved with " ++ lname ++ " do not match original") ldata lout
            rout <- hoistEither $ bondWrite rproto left
            checkEqual ("value saved with " ++ rname ++ " do not match original") rdata rout
    crossTest left right  = crossTest (fromTagged left) (fromTagged right)

    crossTestTagged (TaggedProtoWrapper lname lproto lfile) (TaggedProtoWrapper rname rproto rfile)
        = testCase ("w/o schema: " ++ lname ++ " - " ++ rname) $ assertEither $ do
            ldata <- readData (compatDataPath </> lfile)
            left <- hoistEither $ bondReadTagged lproto ldata
            rdata <- readData (compatDataPath </> rfile)
            right <- hoistEither $ bondReadTagged rproto rdata
            checkEqual "values do not match" left right
            lout <- hoistEither $ bondWriteTagged lproto right
            checkEqual ("value saved with " ++ lname ++ " do not match original") ldata lout
            rout <- hoistEither $ bondWriteTagged rproto left
            checkEqual ("value saved with " ++ rname ++ " do not match original") rdata rout
    crossTestTagged _ _ = error "invalid protocols"

readCompat :: BondProto t => t -> String -> Assertion
readCompat p f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    s <- hoistEither (bondRead p dat :: Either String Compat)
    d' <- hoistEither $ bondWrite p s
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
    schemadef <- hoistEither (bondUnmarshal schemadat :: Either String SchemaDef)
    schema <- hoistEither $ parseSchema schemadef
    dat <- readData (compatDataPath </> f)
    s <- hoistEither $ bondReadWithSchema p schema dat
    outdat <- hoistEither $ bondWriteWithSchema p schema s
    checkEqual "serialized value do not match original" dat outdat

readSchema :: Assertion
readSchema = assertEither $ do
    dat <- readData (compatDataPath </> "compat.schema.dat")
    s <- hoistEither (bondUnmarshal dat :: Either String SchemaDef)
    d' <- hoistEither $ bondMarshal CompactBinaryV1Proto s
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
    dat <- BL.readFile (compatDataPath </> f)
    let parse = bondRead JsonProto dat :: Either String Compat
    case parse of
        Left msg -> assertFailure msg >> return BL.empty
        Right s -> case bondWrite JsonProto s of
            Left msg -> assertFailure msg >> return BL.empty 
            Right out -> return out

testJsonWithSchema :: String -> FilePath -> TestTree
testJsonWithSchema name f = goldenVsString name (compatDataPath </> "golden.json.dat") $
    eitherT (\ msg -> assertFailure msg >> return BL.empty) return $ do
        let schema = getSchema (Proxy :: Proxy Compat)
        dat <- readData (compatDataPath </> f)
        s <- hoistEither $ bondReadWithSchema JsonProto schema dat
        hoistEither $ bondWriteWithSchema JsonProto schema s

testJsonWithRuntimeSchema :: String -> FilePath -> TestTree
testJsonWithRuntimeSchema name f = goldenVsString name (compatDataPath </> "golden.json.dat") $
    eitherT (\ msg -> assertFailure msg >> return BL.empty) return $ do
        schemadat <- readData (compatDataPath </> "compat.schema.dat")
        schemadef <- hoistEither (bondUnmarshal schemadat :: Either String SchemaDef)
        schema <- hoistEither $ parseSchema schemadef
        dat <- readData (compatDataPath </> f)
        s <- hoistEither $ bondReadWithSchema JsonProto schema dat
        hoistEither $ bondWriteWithSchema JsonProto schema s

readAsType :: forall t a. (Show a, BondProto t, BondStruct a) => t -> Proxy a -> String -> Assertion
readAsType p _ f = assertEither $ do
    dat <- readData (compatDataPath </> f)
    void $ hoistEither (bondRead p dat :: Either String a)

matchCompatSchemaDef :: Assertion
matchCompatSchemaDef = assertEither $ do
    dat <- readData (compatDataPath </> "compat.schema.dat")
    fileSchema <- hoistEither $ bondUnmarshal dat
    let schema = getSchema (Proxy :: Proxy Compat)
    let assembledSchema = assembleSchema schema
    checkEqual "schemas do not match" fileSchema assembledSchema

-- gbc's json schemas differ slightly from bond.bond definition,
-- so tests can't compare json representations.
matchGeneratedSchemaDef :: Assertion
matchGeneratedSchemaDef = assertEither $ do
    dat <- readData (autogenPath </> "bond.SchemaDef.json")
    fileSchema <- hoistEither $ bondRead JsonProto dat
    let schema = getSchema (Proxy :: Proxy SchemaDef)
    let assembledSchema = assembleSchema schema
    checkEqual "schemas do not match" fileSchema assembledSchema

checkSchemaError :: FilePath -> IO String
checkSchemaError f = assertWithMsg $ do
    dat <- readData (brokenSchemasPath </> f)
    schema <- hoistEither $ bondRead JsonProto dat
    checkHasError $ parseSchema schema

checkSchemaMismatch :: BondStruct a => Proxy a -> Struct -> IO String
checkSchemaMismatch a s = assertWithMsg $ do
    let schema = getSchema a
    checkHasError $ checkStructSchema schema s

checkShallowSchema :: Assertion
checkShallowSchema = assertEither $ do
    let schema = getSchema (Proxy :: Proxy Outer)
    void $ hoistEither $ checkStructSchema schema $ Struct (Just $ Struct Nothing M.empty) M.empty

testInvalidTaggedWrite :: BondTaggedProto t => t -> Struct -> IO String
testInvalidTaggedWrite p s = assertWithMsg $ checkHasError $ bondWriteTagged p s

failToSaveDefaultNothing :: BondProto t => t -> IO String
failToSaveDefaultNothing p =
    let struct = Struct (Just $ Struct Nothing M.empty)
            $ M.fromList [(Ordinal 13, UINT8 0), (Ordinal 16, INT32 0)]
        schema = getSchema (Proxy :: Proxy BasicTypes)
     in assertWithMsg $ checkHasError $ bondWriteWithSchema p schema struct

failToReadMissingRequired :: BondTaggedProto t => t -> IO String
failToReadMissingRequired p = assertWithMsg $ do
    let struct = Struct Nothing M.empty
    out <- hoistEither $ bondWriteTagged p struct
    checkHasError $ (bondRead p out :: Either String Reqopt)

jsonFailToReadMissingRequired :: IO String
jsonFailToReadMissingRequired = assertWithMsg $ do
    let dat = BL8.pack "{}"
    checkHasError $ (bondRead JsonProto dat :: Either String Reqopt)

failToReadMissingRequiredWithSchema :: BondTaggedProto t => t -> IO String
failToReadMissingRequiredWithSchema p = assertWithMsg $ do
    let struct = Struct Nothing M.empty
    out <- hoistEither $ bondWriteTagged p struct
    let schema = getSchema (Proxy :: Proxy Reqopt)
    checkHasError $ bondReadWithSchema p schema out

failToWriteRequiredNothing :: BondProto t => t -> IO String
failToWriteRequiredNothing p = assertWithMsg $ do
    let struct = defaultValue :: Reqopt
    checkHasError $ bondWrite p struct

failToWriteRequiredNothingWithSchema :: BondTaggedProto t => t -> IO String
failToWriteRequiredNothingWithSchema p = assertWithMsg $ do
    let struct = Struct Nothing $ M.fromList [(Ordinal 10, INT8 5)]
    let schema = getSchema (Proxy :: Proxy Reqopt)
    checkHasError $ bondWriteWithSchema p schema struct

recodeFromTo :: forall t1 t2. (BondProto t1, BondProto t2) => t1 -> t2 -> Assertion
recodeFromTo t1 t2 = assertEither $ do
    let inner = defaultValue{ Inner.m_uint64 = 5, Inner.m_uint32 = [12, 34] } :: Inner
    bonded <- hoistEither $ marshalValue t1 inner
    let outer = defaultValue{ m_bonded_inner = bonded }
    saved <- hoistEither $ bondWrite t2 outer
    v <- hoistEither (bondRead t2 saved :: Either String Outer)
    unpacked <- hoistEither $ getValue (m_bonded_inner v)
    checkEqual "deserialized value do not match with original" inner unpacked

checkCompileIdentity :: Assertion
checkCompileIdentity = assertEither $ do
    schemadat <- readData (compatDataPath </> "compat.schema.dat")
    schemadef <- hoistEither (bondUnmarshal schemadat :: Either String SchemaDef)
    parsedSchema <- hoistEither $ parseSchema schemadef
    let assembledSchema = assembleSchema parsedSchema
    checkEqual "original and decompiled schema do not match" schemadef assembledSchema
--    reparsedSchema <- hoistEither $ parseSchema assembledSchema
--    checkEqual "original and parsed schema do not match" parsedSchema reparsedSchema

checkDefaultStruct :: Assertion
checkDefaultStruct = do
    let schema = getSchema (Proxy :: Proxy Reqopt)
    let defStruct = defaultStruct schema
    assertEqual "struct is not as expected"
        (Struct Nothing $ M.fromList [(Ordinal 10, INT8 5), (Ordinal 20, STRING "")])
        defStruct

zigzagInt16 :: Int16 -> Bool
zigzagInt16 x = x == fromZigZag (toZigZag x)

zigzagInt32 :: Int32 -> Bool
zigzagInt32 x = x == fromZigZag (toZigZag x)

zigzagInt64 :: Int64 -> Bool
zigzagInt64 x = x == fromZigZag (toZigZag x)

zigzagWord64 :: Word64 -> Bool
zigzagWord64 x = x == toZigZag (fromZigZag x :: Int64)

assertEither :: EitherT String IO () -> Assertion
assertEither = eitherT assertFailure (const $ return ())

assertWithMsg :: EitherT String IO String -> IO String
assertWithMsg = eitherT (\ msg -> assertFailure msg >> return "") (\ msg -> return $ "error caught: " ++ msg)

readData :: FilePath -> EitherT String IO BL.ByteString
readData = lift . BL.readFile

checkEqual :: (Eq a, Show a, MonadTrans t) => String -> a -> a -> t IO ()
checkEqual m a b = lift $ assertEqual m a b

checkHasError :: Either String a -> EitherT String IO String
checkHasError = bimapEitherT (const "error not found") id . swapEitherT . hoistEither
