import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0 
                      then read (head args)
                      else 0 :: Int
    vals <- replicateM linesToRead getLine
    -- let sumOut = foldr ((+) . read) 0 vals
    let ints = map read vals :: [Int]
    print (sum ints)
