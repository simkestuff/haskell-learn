halve :: Int -> Double
halve n = fromIntegral n / 2.0

mytest :: IO Int -> IO Double
mytest n = do
    x <- n 
    let result = halve x 
    return result

myint :: IO Int
myint = do
    x <- getLine
    let r = read x 
    return r

main :: IO ()
main = do
    x <- getLine
    result <- mytest (return (read x))
    print result
