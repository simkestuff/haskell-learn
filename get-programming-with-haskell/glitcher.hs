{-# LANGUAGE PackageImports #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import "random" System.Random

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args 
    imageFile <- BC.readFile fileName 
    glitched <- randomReplaceBytes imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched

int2Char :: Int -> Char
int2Char = toEnum . (`mod` 255) 

int2BC :: Int -> BC.ByteString
-- int2BC = BC.pack . singleton . int2Char
int2BC i = BC.pack [int2Char i]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, glitched, after]
    where (before, _) = BC.splitAt (loc - 1) bytes
          (_, after) = BC.splitAt loc bytes
          glitched = int2BC charVal

randomReplaceBytes :: BC.ByteString -> IO BC.ByteString
randomReplaceBytes bytes = do
    let byteLengths = BC.length bytes
    location <- randomRIO (1, byteLength)
    charVal <- randomRIO (1, 255)
    return (replaceByte location charVal bytes)
