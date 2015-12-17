{-# Language ScopedTypeVariables #-}
import Data.Bond.Schema.SchemaDef
import Data.Bond
import Unittest.Compat.Another.Another
import Unittest.Compat.BasicTypes
import Unittest.Compat.Compat

import Data.Proxy
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
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
--                                    L.writeFile ("/tmp" </> f) d'
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
