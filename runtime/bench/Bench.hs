{-# Language GADTs, ScopedTypeVariables #-}
module Main where

import Data.Bond
import Unittest.Compat.Compat

import Criterion.Main
import Data.Proxy
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

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
    ]

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
