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
              testCase "read/write BasicTypes value" $
                readAsType (Proxy :: Proxy FastBinaryProto) (Proxy :: Proxy BasicTypes) "compat.fast.dat",
              testCase "read/write Another value" $
                readAsType (Proxy :: Proxy FastBinaryProto) (Proxy :: Proxy Another) "compat.fast.dat"
            ],
          testGroup "CompactBinary"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy CompactBinaryProto) "compat.compact2.dat",
              testCase "read/write BasicTypes value" $
                readAsType (Proxy :: Proxy CompactBinaryProto) (Proxy :: Proxy BasicTypes) "compat.compact2.dat",
              testCase "read/write Another value" $
                readAsType (Proxy :: Proxy CompactBinaryProto) (Proxy :: Proxy Another) "compat.compact2.dat"
            ],
          testGroup "CompactBinary v1"
            [ testCase "read/write Compat value" $
                readCompat (Proxy :: Proxy CompactBinaryV1Proto) "compat.compact.dat",
              testCase "read/write BasicTypes value" $
                readAsType (Proxy :: Proxy CompactBinaryV1Proto) (Proxy :: Proxy BasicTypes) "compat.compact.dat",
              testCase "read/write Another value" $
                readAsType (Proxy :: Proxy CompactBinaryV1Proto) (Proxy :: Proxy Another) "compat.compact.dat"
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
        Left (_, used, msg) -> assertFailure $ "parse error at " ++ show used ++ ": " ++ msg
        Right (rest, used, s) -> let _ = s :: Compat -- type binding
                                  in do
                                    assertBool ("incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)) (L.null rest)
                                    let d' = bondWrite p s
                                    L.writeFile ("/tmp" </> f) d'
                                    assertEqual "serialized value do not match original" dat d'

readAsType :: forall t a. (BondProto t, Schemable a, BondStruct a) => Proxy t -> Proxy a -> String -> Assertion
readAsType p _ f = do
    dat <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p dat
    case parse of
        Left (_, used, msg) -> assertFailure $ "parse error at " ++ show used ++ ": " ++ msg
        Right (rest, used, s) -> let _ = s :: a -- type binding
                                  in assertBool ("incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)) (L.null rest)

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
