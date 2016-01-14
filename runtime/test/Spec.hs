{-# Language ScopedTypeVariables #-}
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

main :: IO ()
main = defaultMain tests

defaultDataPath :: String
defaultDataPath = "compat" </> "data"

tests :: TestTree
tests = testGroup "Haskell runtime tests"
    [ testGroup "Runtime schema tests"
        [ testCase "create SchemaDef schema" createSchemaDef,
          testCase "create Compat schema" createCompatSchema
        ],
      testGroup "Compile-time schema tests"
        [ testGroup "SimpleBinary"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy SimpleBinaryProto) "compat.simple2.dat"
            ],
          testGroup "SimpleBinary v1"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy SimpleBinaryV1Proto) "compat.simple.dat"
            ],
          testGroup "FastBinary"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy FastBinaryProto) "compat.fast.dat",
              testCase "read BasicTypes value" $
                readAsType (Proxy :: Proxy FastBinaryProto) (Proxy :: Proxy BasicTypes) "compat.fast.dat",
              testCase "read Another value" $
                readAsType (Proxy :: Proxy FastBinaryProto) (Proxy :: Proxy Another) "compat.fast.dat"
            ],
          testGroup "CompactBinary"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy CompactBinaryProto) "compat.compact2.dat",
              testCase "read BasicTypes value" $
                readAsType (Proxy :: Proxy CompactBinaryProto) (Proxy :: Proxy BasicTypes) "compat.compact2.dat",
              testCase "read Another value" $
                readAsType (Proxy :: Proxy CompactBinaryProto) (Proxy :: Proxy Another) "compat.compact2.dat"
            ],
          testGroup "CompactBinary v1"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy CompactBinaryV1Proto) "compat.compact.dat",
              testCase "read BasicTypes value" $
                readAsType (Proxy :: Proxy CompactBinaryV1Proto) (Proxy :: Proxy BasicTypes) "compat.compact.dat",
              testCase "read Another value" $
                readAsType (Proxy :: Proxy CompactBinaryV1Proto) (Proxy :: Proxy Another) "compat.compact.dat"
            ],
          testGroup "JSON"
            [ testJson "read/write original Compat value" "compat.json.dat",
              testJson "read/write golden Compat value" "golden.json.dat",
              testCase "read BasicTypes value" $
                readAsType (Proxy :: Proxy JsonProto) (Proxy :: Proxy BasicTypes) "compat.json.dat",
              testCase "read Another value" $
                readAsType (Proxy :: Proxy JsonProto) (Proxy :: Proxy Another) "compat.json.dat"
            ],
          testGroup "ZigZag encoding"
            [ testProperty "Int16" zigzagInt16,
              testProperty "Int32" zigzagInt32,
              testProperty "Int64" zigzagInt64,
              testProperty "Word64" zigzagWord64
            ]
        ]
    ]

readCompat :: BondProto t => Proxy t -> String -> Assertion
readCompat p f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> let _ = s :: Compat -- type binding
                    in do
                        let Right d' = bondWrite p s
--                        L.writeFile ("/tmp" </> (f ++ ".out")) d'
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
    let p = Proxy :: Proxy JsonProto
    let parse = bondRead p dat
    case parse of
        Left msg -> do
            assertFailure msg
            return L.empty
        Right s -> let _ = s :: Compat -- type binding
                    in do
                        let Right d' = bondWrite p s
                        return d'

readAsType :: forall t a. (BondProto t, BondStruct a) => Proxy t -> Proxy a -> String -> Assertion
readAsType p _ f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat
    case parse of
        Left msg -> assertFailure msg
        Right s -> let _ = s :: a -- type binding
                    in return ()

createSchemaDef :: Assertion
createSchemaDef = do
    let schema = getSchema (Proxy :: Proxy SchemaDef)
    let reprlen = length $ show schema
    -- the point of this test is to force schema creation and see if it is finite
    assertBool "impossible schema length 0" (reprlen > 0)

createCompatSchema :: Assertion
createCompatSchema = do
    let schema = getSchema (Proxy :: Proxy Compat)
    let reprlen = length $ show schema
    -- the point of this test is to force schema creation and see if it is finite
    assertBool "impossible schema length 0" (reprlen > 0)

zigzagInt16 :: Int16 -> Bool
zigzagInt16 x = x == (fromZigZag $ toZigZag x)

zigzagInt32 :: Int32 -> Bool
zigzagInt32 x = x == (fromZigZag $ toZigZag x)

zigzagInt64 :: Int64 -> Bool
zigzagInt64 x = x == (fromZigZag $ toZigZag x)

zigzagWord64 :: Word64 -> Bool
zigzagWord64 x = x == (toZigZag $ (fromZigZag x :: Int64))
