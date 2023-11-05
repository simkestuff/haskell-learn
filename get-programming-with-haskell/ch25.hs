{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC 

sampleBytes :: B.ByteString
sampleBytes = "Hello"

sampleString :: String
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

qc251 :: BC.ByteString -> Int 
qc251 = read . BC.unpack


