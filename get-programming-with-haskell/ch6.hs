isPalindrome xs = xs == reverse xs 

-- Q6.1

myRepeat :: a -> [a]
myRepeat x = cycle [x]

-- Q6.2

subseq start end xs = take howMany (drop start xs)
    where howMany = end - start

-- Q6.3

inFirstHalf :: Eq a => [a] -> a -> Bool
inFirstHalf xs x = x `elem` firstHalf
    where  half = div (length xs) 2
           firstHalf = take half xs
