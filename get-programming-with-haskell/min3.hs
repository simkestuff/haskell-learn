minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x (min y z)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

printResult :: IO Int -> IO ()
printResult x = do
    res <- x
    print res


main :: IO ()
main = printResult minOfInts
