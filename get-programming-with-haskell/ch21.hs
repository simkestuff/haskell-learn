fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1 
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 c = fastFib n2 (n1+n2) (c-1)

fib :: Int -> Int
fib = fastFib 1 1

main :: IO ()
main = do
    putStrLn "Unesi n"
    n <- getLine
    let range = [1 .. read n :: Int]
    let fibs = show (map fib range)
    putStrLn fibs
    

