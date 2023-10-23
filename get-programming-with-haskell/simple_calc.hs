sampleEq = "2+4\n9*8"

solveEq :: String -> Int
solveEq cs = firstOp `op` secondOp
    where firstOp = read (takeWhile (not . isOperation) cs)
          -- op = ops (head (dropWhile (not . isOperation) cs))
          op = (ops . head . dropWhile (not . isOperation)) cs
          -- secondOp = read (tail (dropWhile (not . isOperation) cs))
          secondOp = (read . tail . dropWhile (not . isOperation)) cs
          

ops :: Num a => Char -> (a -> a -> a)
ops '+' = (+)
ops '*' = (*)

isOperation :: Char -> Bool
isOperation c = c == '+' || c == '*'

solve :: String -> [Int] 
solve = map solveEq . lines


main :: IO ()
main = do
    eqs <- getContents
    mapM_ print (solve eqs)
