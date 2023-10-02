import Data.Char

addAnA [] = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

remove _ [] = []
remove test (x:xs) = if test x 
                     then remove test xs 
                     else x : remove test xs


mf f v [] = v
mf f v (x:xs) = mf f (f v x) xs


myelem x xs = if length (filter (== x) xs) > 0
              then True
              else False


isPalindrome xs = xs' == reverse xs'
    where xs' = clean xs

clean xs = filter (/= ' ') (map replaceCapitals xs)
    where replaceCapitals x = if x `elem` ['A' .. 'Z']
                              then toLower x 
                              else x

harmonic n = foldl (\x y -> x + 1 / y) 0 [1 .. n]

harmonic2 n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
          seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs
