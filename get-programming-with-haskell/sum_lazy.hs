
main :: IO ()
main = do
    args <- getContents
    let sumOfInts = sum (toInts args)
    let ss = sumOfSquares (toInts args)
    print sumOfInts
    print ss

sampleData = ['6','2','\n','2','1','\n']

getListOfNumbers :: [Char] -> [Int]
getListOfNumbers [] = []
getListOfNumbers cs = getNumber : getListOfNumbers restOfList
    where notNewline = (/= '\n')
          restOfList = tail (dropWhile notNewline cs)
          getNumber = read (takeWhile notNewline cs) :: Int

sumNumbers :: [Char] -> Int
sumNumbers [] = 0
sumNumbers xs = sum (getListOfNumbers xs)

toInts :: String -> [Int]
toInts = map read . lines

sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum (map (^2) xs)
