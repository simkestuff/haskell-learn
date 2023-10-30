{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
--import System.Environment
--
-- getCounts :: String -> (Int, Int, Int)
-- getCounts [] = (0, 0, 0)
-- getCounts s = (charCount, wordCount, lineCount)
--     where charCount = length s 
--           wordCount = (length . words) s
--           lineCount = (length . lines) s
--
-- countsText :: (Int, Int, Int) -> String
-- countsText (0, 0, 0) = "Empty"
-- countsText (c, w, l) = unwords ["Chars:",
--                                 show c, 
--                                 ", Words:", 
--                                 show w, 
--                                 ", Lines:",
--                                 show l]
--
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let inputFile = head args
--     input <- readFile inputFile
--     let output = (countsText . getCounts) input
--     appendFile "stat.dat" (mconcat ["Summary: ", inputFile, " -> ", output, "\n"])
   
getCounts :: T.Text -> (Int, Int, Int)
getCounts s = (charCount, wordCount, lineCount)
    where charCount = T.length s 
          wordCount = (length . T.words) s
          lineCount = (length . T.lines) s

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) = T.unwords ["Chars:",
                                   (T.pack . show) cc,
                                   "words:",
                                   (T.pack . show) wc,
                                   "lines:",
                                   (T.pack . show) lc]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args 
    input <- TIO.readFile inputFile
    let output = (countsText . getCounts) input
    TIO.appendFile "stat.dat" (T.pack (mconcat ["Summary ", inputFile, " -> ", T.unpack output, "\n"]))
