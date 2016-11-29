import Unittest.Simple.Service
import Unittest.Simple.Gateway
import Unittest.Simple.Watchdog
import Unittest.Calculator.Calculator

import Data.Bond
import Network.Bond
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Haskell RPC tests"
    [
    ]
