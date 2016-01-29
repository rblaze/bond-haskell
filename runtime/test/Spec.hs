{-# Language ScopedTypeVariables #-}
import Data.Bond
import Data.Bond.Marshal
import Data.Bond.ZigZag
import Data.Bond.Schema.SchemaDef
import Unittest.Compat.Another.Another
import Unittest.Compat.BasicTypes
import Unittest.Compat.Compat

import Data.Int
import Data.Proxy
import Data.Word
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as L

import DataPath

main :: IO ()
main = defaultMain tests

defaultDataPath :: String
defaultDataPath = "compat" </> "data"

tests :: TestTree
tests = testGroup "Haskell runtime tests"
    [ testGroup "Runtime schema tests"
        [ testCase "check saved Compat schema matching our schema" matchCompatSchemaDef,
          testCase "check gbc-generated SchemaDef schema matching our schema" matchGeneratedSchemaDef
        ],
      testGroup "Protocol tests"
        [ testGroup "SimpleBinary"
            [ testCase "read/write Compat value" $
                readCompat SimpleBinaryProto "compat.simple2.dat"
            ],
          testGroup "SimpleBinary v1"
            [ testCase "read/write Compat value" $
                readCompat SimpleBinaryV1Proto "compat.simple.dat"
            ],
          testGroup "FastBinary"
            [ testCase "read/write Compat value" $
                readCompat FastBinaryProto "compat.fast.dat",
              testCase "read BasicTypes value" $
                readAsType FastBinaryProto (Proxy :: Proxy BasicTypes) "compat.fast.dat",
              testCase "read Another value" $
                readAsType FastBinaryProto (Proxy :: Proxy Another) "compat.fast.dat",
              testCase "read Compat w/o schema" $
                readCompatSchemaless FastBinaryProto "compat.fast.dat"
            ],
          testGroup "CompactBinary"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryProto "compat.compact2.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryProto (Proxy :: Proxy BasicTypes) "compat.compact2.dat",
              testCase "read Another value" $
                readAsType CompactBinaryProto (Proxy :: Proxy Another) "compat.compact2.dat",
              testCase "read Compat w/o schema" $
                readCompatSchemaless CompactBinaryProto "compat.compact2.dat"
            ],
          testGroup "CompactBinary v1"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryV1Proto "compat.compact.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy BasicTypes) "compat.compact.dat",
              testCase "read Another value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy Another) "compat.compact.dat",
              testCase "read Compat w/o schema" $
                readCompatSchemaless CompactBinaryV1Proto "compat.compact.dat"
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
            ldat <- L.readFile (defaultDataPath </> lfile)
            let lparse = lreader ldat
            whenLeft lparse assertFailure
            rdat <- L.readFile (defaultDataPath </> rfile)
            let rparse = rreader rdat
            whenLeft rparse assertFailure
            assertEqual "values do not match" lparse rparse

readCompat :: BondProto t => t -> String -> Assertion
readCompat p f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat :: Either String Compat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let d' = bondWrite p s
--                    L.writeFile ("/tmp" </> (f ++ ".out")) d'
                    assertEqual "serialized value do not match original" dat d'

readCompatSchemaless :: BondTaggedProto t => t -> String -> Assertion
readCompatSchemaless p f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondReadTagged p dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let d' = bondWriteTagged p s
                    L.writeFile ("/tmp" </> (f ++ ".out")) d'
                    assertEqual "serialized value do not match original" dat d'


readSchema :: Assertion
readSchema = do
    dat <- L.readFile (defaultDataPath </> "compat.schema.dat")
    let parse = bondUnmarshal dat :: Either String SchemaDef
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let d' = bondMarshal CompactBinaryV1Proto s
--                    L.writeFile ("/tmp" </> (f ++ ".out")) d'
                    assertEqual "serialized value do not match original" dat d'

readSchemaTagged :: Assertion
readSchemaTagged = do
    dat <- L.readFile (defaultDataPath </> "compat.schema.dat")
    let parse = bondUnmarshalTagged dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let d' = bondMarshalTagged CompactBinaryV1Proto s
--                    L.writeFile ("/tmp" </> (f ++ ".out")) d'
                    assertEqual "serialized value do not match original" dat d'

-- compat.json.dat file has several properties making it incompatible with usual test:
-- all float values saved with extra precision
-- double values are saved in a way different from one used by scientific
-- fields are saved in declaration order, while aeson saves them in pseudorandom hash order
-- optional fields with default values are present

-- Golden file is manually checked to match compat data as best as it could

testJson :: String -> FilePath -> TestTree
testJson name f = goldenVsString name (defaultDataPath </> "golden.json.dat") $ do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead JsonProto dat :: Either String Compat
    case parse of
        Left msg -> do
            assertFailure msg
            return L.empty
        Right s -> do
                    let d' = bondWrite JsonProto s
                    return d'

readAsType :: forall t a. (Show a, BondProto t, BondStruct a) => t -> Proxy a -> String -> Assertion
readAsType p _ f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat :: Either String a
    whenLeft parse assertFailure

matchCompatSchemaDef :: Assertion
matchCompatSchemaDef = do
    dat <- L.readFile (defaultDataPath </> "compat.schema.dat")
    let parse = bondUnmarshal dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let s' = getSchema (Proxy :: Proxy Compat)
                    assertEqual "schemas do not match" s s'

-- gbc's json schemas slightly differ from bond.bond definition,
-- so I can't compare json representations.
matchGeneratedSchemaDef :: Assertion
matchGeneratedSchemaDef = do
    dat <- L.readFile (autogenPath </> "bond.SchemaDef.json")
    let parse = bondRead JsonProto dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let s' = getSchema (Proxy :: Proxy SchemaDef)
                    assertEqual "schemas do not match" s s'

zigzagInt16 :: Int16 -> Bool
zigzagInt16 x = x == (fromZigZag $ toZigZag x)

zigzagInt32 :: Int32 -> Bool
zigzagInt32 x = x == (fromZigZag $ toZigZag x)

zigzagInt64 :: Int64 -> Bool
zigzagInt64 x = x == (fromZigZag $ toZigZag x)

zigzagWord64 :: Word64 -> Bool
zigzagWord64 x = x == (toZigZag $ (fromZigZag x :: Int64))

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Right _) _ = return ()
whenLeft (Left a) f = f a
