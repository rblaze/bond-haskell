import Data.Bond.Schema.SchemaDef
import Data.Bond
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
            [ testCase "read/write compat value" $
                readCompat (Proxy :: Proxy SimpleBinaryProto) "compat.simple2.dat"
            ],
          testGroup "FastBinary"
            [ testCase "read/write compat value" $
                readCompat (Proxy :: Proxy FastBinaryProto) "compat.fast.dat"
            ],
          testGroup "CompactBinary"
            [ testCase "read/write compat value" $
                readCompat (Proxy :: Proxy CompactBinaryProto) "compat.compact2.dat"
            ]
        ]
    ]

readCompat :: BondProto t => Proxy t -> String -> Assertion
readCompat p f = do
    d <- L.readFile (defaultDataPath </> f)
    let parse = bondRead p d
    case parse of
        Left (_, used, msg) -> assertFailure $ "parse error at " ++ show used ++ ": " ++ msg
        Right (rest, used, s) -> let _ = s :: Compat -- type binding
                                  in do
                                    assertBool ("incomplete parse, used " ++ show used ++ ", left " ++ show (L.length rest)) (L.null rest)
                                    let d' = bondWrite p s
--                                    L.writeFile ("/tmp" </> f) d'
                                    assertEqual "serialized value do not match original" d d'

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
