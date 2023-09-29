myl [] = 0
myl (x:xs) = 1 + myl xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs 

-- Q8.1
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q8.2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1 
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 c = fastFib n2 (n1+n2) (c-1)

fib = fastFib 1 1

