-- myGCD a b = if remainder == 0
--             then b
--             else myGCD b remainder
--     where remainder = a `mod` b

-- QC7.3
myTail (_:xs) = xs 
myTail [] = []

-- Q7.2
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
