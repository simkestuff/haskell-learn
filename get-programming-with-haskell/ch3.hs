-- sumOfSquaresOrSquareSum x y = if sumSquare > squareSum 
--                               then sumSquare 
--                               else squareSum 
--     where sumSquare = x ** 2 + y ** 2 
--           squareSum = (x + y) ** 2

body sumSquare squareSum = max sumSquare squareSum

-- sumOfSquaresOrSquareSum x y = body (x^2 + y^2) ((x+y)^2) 
--

-- sumOfSquaresOrSquareSum x y = (\sumSquare squareSum -> max sumSquare squareSum) (x^2 + y^2) ((x+y)^2) 

-- doubleDouble x = doubs * 2 
--     where doubs = x * 2
--
doubleDouble x = (\doubs -> doubs * 2) (x*2)

sumOfSquaresOrSquareSum x y = let sumSquare = x^2 + y^2 
                                  squareSum = (x + y)^2
                              in 
                                max sumSquare squareSum



overwrite x = let x = 2
              in
                let x = 3
                in
                  let x = 4
                  in
                    x

overwrite2 x = (\x -> 
                (\x -> 
                (\x -> x) 4) 3) 2

x = 4

add1 y = x + y 

add2 y = (\x -> x + y) 3

add3 y = (\y -> 
          (\x -> y + x) 1) 2
          
          