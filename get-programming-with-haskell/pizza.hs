import Distribution.Simple.PreProcess (ppC2hs)
type Pizza = (Double, Double)

areaOfPizza :: Double -> Double
areaOfPizza d = r^2 * 3.14
    where r = d / 2

costPerInch :: Pizza -> Double
costPerInch pizza = cost / areaOfPizza size 
    where (size, cost) = pizza
          
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 
    | costPerInch p1 < costPerInch p2 = p1 
    | costPerInch p2 < costPerInch p1 = p2 
    | otherwise = p1

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
   where costSqInch = costPerInch (size,cost)

main :: IO ()
main = do
    putStrLn "Enter size of pizza one: "
    s1 <- getLine
    let size1 = read s1 :: Double
    putStrLn "Enter price od pizza one:  "
    p1 <- getLine
    let price1 = read p1 :: Double
    putStrLn "Enter size of pizza two: "
    s2 <- getLine
    let size2 = read s2 :: Double
    putStrLn "Enter price od pizza one:  "
    p2 <- getLine
    let price2 = read p2 :: Double
    if comparePizzas (size1, price1) (size2, price2) == (size1, price1)
    then putStrLn (describePizza (size1, price1))
    else putStrLn (describePizza (size2, price2))
