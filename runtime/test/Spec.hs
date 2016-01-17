{-# Language OverloadedStrings, ScopedTypeVariables #-}
import Data.Bond
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
          testCase "check generated SchemaDef matching our" matchGeneratedSchemaDef
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
                readAsType FastBinaryProto (Proxy :: Proxy Another) "compat.fast.dat"
            ],
          testGroup "CompactBinary"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryProto "compat.compact2.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryProto (Proxy :: Proxy BasicTypes) "compat.compact2.dat",
              testCase "read Another value" $
                readAsType CompactBinaryProto (Proxy :: Proxy Another) "compat.compact2.dat"
            ],
          testGroup "CompactBinary v1"
            [ testCase "read/write Compat value" $
                readCompat CompactBinaryV1Proto "compat.compact.dat",
              testCase "read BasicTypes value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy BasicTypes) "compat.compact.dat",
              testCase "read Another value" $
                readAsType CompactBinaryV1Proto (Proxy :: Proxy Another) "compat.compact.dat"
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
            [ testCase "read/write SchemaDef value" readSchema
            ]
        ],
      testGroup "ZigZag encoding"
        [ testProperty "Int16" zigzagInt16,
          testProperty "Int32" zigzagInt32,
          testProperty "Int64" zigzagInt64,
          testProperty "Word64" zigzagWord64
        ]
    ]

readCompat :: BondProto t => t -> String -> Assertion
readCompat p f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let _ = s :: Compat -- type binding
                    let Right d' = bondWrite p s
--                    L.writeFile ("/tmp" </> (f ++ ".out")) d'
                    assertEqual "serialized value do not match original" dat d'

readSchema :: Assertion
readSchema = do
    dat <- L.readFile (defaultDataPath </> "compat.schema.dat")
    let parse = bondReadMarshalled dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let _ = s :: SchemaDef -- type binding
                    let Right d' = bondWriteMarshalled CompactBinaryV1Proto s
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
    let parse = bondRead JsonProto dat
    case parse of
        Left msg -> do
            assertFailure msg
            return L.empty
        Right s -> do
                    let _ = s :: Compat -- type binding
                    let Right d' = bondWrite JsonProto s
                    return d'

readAsType :: forall t a. (BondProto t, BondStruct a) => t -> Proxy a -> String -> Assertion
readAsType p _ f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> let _ = s :: a -- type binding
                    in return ()

matchCompatSchemaDef :: Assertion
matchCompatSchemaDef = do
    dat <- L.readFile (defaultDataPath </> "compat.schema.dat")
    let parse = bondReadMarshalled dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let _ = s :: SchemaDef -- type binding
                    let s' = getSchema (Proxy :: Proxy Compat)
                    assertEqual "schemas do not match" (show s) (show s')

-- gbc's json schemas slightly differ from bond.bond definition,
-- so I can't compare json representations.
matchGeneratedSchemaDef :: Assertion
matchGeneratedSchemaDef = do
    dat <- L.readFile (autogenPath </> "bond.SchemaDef.json")
    let parse = bondRead JsonProto dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> do
                    let _ = s :: SchemaDef -- type binding
                    let s' = getSchema (Proxy :: Proxy SchemaDef)
                    assertEqual "schemas do not match" (show s) (show s')

zigzagInt16 :: Int16 -> Bool
zigzagInt16 x = x == (fromZigZag $ toZigZag x)

zigzagInt32 :: Int32 -> Bool
zigzagInt32 x = x == (fromZigZag $ toZigZag x)

zigzagInt64 :: Int64 -> Bool
zigzagInt64 x = x == (fromZigZag $ toZigZag x)

zigzagWord64 :: Word64 -> Bool
zigzagWord64 x = x == (toZigZag $ (fromZigZag x :: Int64))
