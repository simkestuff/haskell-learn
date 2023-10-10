data Shape = Circle Double | Square Double | Rectangle Double Double

perimetar :: Shape -> Double
perimetar (Circle r) = 2 * 3.14 * r
perimetar (Square a) = 4 * a
perimetar (Rectangle a b) = 2 * a + 2 * b

area :: Shape -> Double
area (Circle r) = r^2 * 3.14
area (Square a) = a^2 
area (Rectangle a b) = a * b
