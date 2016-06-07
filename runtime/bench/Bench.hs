{-# Language GADTs, ScopedTypeVariables #-}
module Main where

import Data.Bond
import Benchmark.Intvec
import Unittest.Compat.Compat

import Criterion.Main
import Data.Proxy
import Data.Word
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

compatDataPath :: String
compatDataPath = "test" </> "compat" </> "data"

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

main :: IO ()
main = defaultMain
    [ bgroup "bondRead" $ map benchRead (simpleProtocols ++ taggedProtocols)
    , bgroup "bondReadTagged" $ map benchReadTagged taggedProtocols
    , bgroup "bondReadWithSchema" $ map benchReadSchema (simpleProtocols ++ taggedProtocols)
    , bgroup "bondWrite" $ map benchWrite (simpleProtocols ++ taggedProtocols)
    , bgroup "bondWriteTagged" $ map benchWriteTagged taggedProtocols
    , bgroup "bondWriteWithSchema" $ map benchWriteSchema (simpleProtocols ++ taggedProtocols)
    , bgroup "VarInt"
        [ bgroup "Read"
            [ intRead "1" $ take 10000 $ cycle [0 .. 127]
            , intRead "2" $ take 10000 $ cycle [128 .. 16383]
            , intRead "3" $ take 10000 $ cycle [16384 .. 2097151]
            , intRead "4" $ take 10000 $ cycle [2097152 .. 268435455]
            , intRead "5" $ take 10000 $ cycle [268435456 .. 34359738367]
            , intRead "6" $ take 10000 $ cycle [34359738368 .. 4398046511103]
            , intRead "7" $ take 10000 $ cycle [4398046511104 .. 562949953421311]
            , intRead "8" $ take 10000 $ cycle [562949953421312 .. 72057594037927935]
            , intRead "9" $ take 10000 $ cycle [72057594037927936 .. 9223372036854775807]
            , intRead "10" $ take 10000 $ cycle [9223372036854775808 .. maxBound]
            ]
        , bgroup "Write"
            [ intWrite "1" $ take 10000 $ cycle [0 .. 127]
            , intWrite "2" $ take 10000 $ cycle [128 .. 16383]
            , intWrite "3" $ take 10000 $ cycle [16384 .. 2097151]
            , intWrite "4" $ take 10000 $ cycle [2097152 .. 268435455]
            , intWrite "5" $ take 10000 $ cycle [268435456 .. 34359738367]
            , intWrite "6" $ take 10000 $ cycle [34359738368 .. 4398046511103]
            , intWrite "7" $ take 10000 $ cycle [4398046511104 .. 562949953421311]
            , intWrite "8" $ take 10000 $ cycle [562949953421312 .. 72057594037927935]
            , intWrite "9" $ take 10000 $ cycle [72057594037927936 .. 9223372036854775807]
            , intWrite "10" $ take 10000 $ cycle [9223372036854775808 .. maxBound]
            ]
        ]
    ]

intRead :: String -> [Word64] -> Benchmark
intRead name xs = env
    makeStream
    (bench name . nf (bondRead CompactBinaryProto :: BL.ByteString -> Either String Intvec))
    where
    makeStream = do
        let v = defaultValue { ints = V.fromList xs }
        let Right stream = bondWrite CompactBinaryProto v
        return stream

intWrite :: String -> [Word64] -> Benchmark
intWrite name xs = env
    (return defaultValue { ints = V.fromList xs })
    (bench name . nf (bondWrite CompactBinaryProto))

benchRead :: ProtoWrapper -> Benchmark
benchRead (ProtoWrapper n t dat) =
    env (BL.fromStrict <$> BS.readFile (compatDataPath </> dat))
        (bench n . nf (bondRead t :: BL.ByteString -> Either String Compat))
benchRead p = benchRead (fromTagged p)

benchReadTagged :: ProtoWrapper -> Benchmark
benchReadTagged (TaggedProtoWrapper n t dat) =
    env (BL.fromStrict <$> BS.readFile (compatDataPath </> dat))
        (bench n . nf (bondReadTagged t))
benchReadTagged _ = error "wrong type"

benchReadSchema :: ProtoWrapper -> Benchmark
benchReadSchema (ProtoWrapper n t dat) =
    let schema = getSchema (Proxy :: Proxy Compat)
    in env (do
            stream <- BL.fromStrict <$> BS.readFile (compatDataPath </> dat)
            -- run decoding once to expand schema
            return (bondReadWithSchema t schema stream `seq` stream)
           )
           (bench n . nf (bondReadWithSchema t schema))
benchReadSchema p = benchReadSchema (fromTagged p)

benchWrite :: ProtoWrapper -> Benchmark
benchWrite (ProtoWrapper n t dat) =
    env getCompat
        (bench n . nf (bondWrite t))
    where
    getCompat :: IO Compat
    getCompat = do
        stream <- BL.readFile (compatDataPath </> dat)
        let Right val = bondRead t stream
        return val
benchWrite p = benchWrite (fromTagged p)

benchWriteTagged :: ProtoWrapper -> Benchmark
benchWriteTagged (TaggedProtoWrapper n t dat) =
    env getStruct
        (bench n . nf (bondWriteTagged t))
    where
    getStruct = do
        stream <- BL.readFile (compatDataPath </> dat)
        let Right val = bondReadTagged t stream
        return val
benchWriteTagged _ = error "wrong type"

benchWriteSchema :: ProtoWrapper -> Benchmark
benchWriteSchema (ProtoWrapper n t dat) =
    env getStruct
        (bench n . nf (bondWriteWithSchema t schema))
    where
    schema = getSchema (Proxy :: Proxy Compat)
    getStruct = do
        stream <- BL.readFile (compatDataPath </> dat)
        let Right val = bondReadWithSchema t schema stream
        return val
benchWriteSchema p = benchWriteSchema (fromTagged p)
