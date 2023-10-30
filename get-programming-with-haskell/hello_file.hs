import System.IO

main :: IO ()
main = do
    fh <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine fh 
    putStrLn firstLine
    secondLine <- hGetLine fh 
    secFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn secFile secondLine
    hClose fh 
    hClose secFile
    putStrLn "Done!"
