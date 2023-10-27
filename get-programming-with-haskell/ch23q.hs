{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "!"]

-- main :: IO ()
-- main = do
--     TIO.putStrLn "What is your name?"
--     name <- TIO.getLine
--     let statement = helloPerson name
--     TIO.putStrLn statement

toInts :: T.Text -> [Int]
toInts = map (read. T.unpack) . T.lines

main :: IO ()
main = do
    userInput <- TIO.getContents
    let numbers = toInts userInput
    print (sum numbers)
