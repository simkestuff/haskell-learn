myAverage aList = sum aList / fromIntegral (length aList)

makeAddress :: Int -> String -> (Int, String)
makeAddress = \number -> \street -> (number, street)

testIt :: a -> b -> a -> (a, b, a)
testIt x y z = (x, y, z)

-- filter :: (a -> Bool) -> [a] -> [a]
-- head :: [a] -> a
-- tail :: [a] -> [a]
--
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x
