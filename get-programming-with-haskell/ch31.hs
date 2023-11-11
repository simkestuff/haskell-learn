import qualified Data.Map as Map 

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM mp = mp >>= \(x,y) -> return (max x y)

test :: (Num a, Ord a) => IO (a,a)
test = return (3,9)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"
--
-- main :: IO ()
-- main = do
--     name <- getLine
--     let statement = helloPerson name
--     putStrLn statement

-- main = getLine >>= 
--        (\name -> 
--             (\statement -> 
--                 putStrLn statement) (helloPerson name)) 


-- echo = do
--     tt <- getLine
--     putStrLn tt
echo = getLine >>=
        \tt -> putStrLn tt


-------------------------------------------------------
-- questions
-- ----------------------------------------------------

-- main :: IO ()
-- main = do
--     putStrLn "What is the size of pizza 1"
--     size1 <- getLine
--     putStrLn "What is the cost of pizza 1"
--     cost1 <- getLine
--     putStrLn "What is the size of pizza 2"
--     size2 <- getLine
--     putStrLn "What is the cost of pizza 2"
--     cost2 <- getLine
--     let pizza1 = (read size1, read cost1)
--     let pizza2 = (read size2, read cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     putStrLn (describePizza betterPizza)

type Pizza = (Double, Double)

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas = undefined

describePizza :: Pizza -> String
describePizza = undefined

main = putStrLn "What is the size of pizza 1" >>
       getLine >>= 
       (\size1 -> 
        putStrLn "What is cost of pizza 1" >>
        getLine >>= 
        (\cost1 -> 
         putStrLn "What is the size of pizza 2" >>
         getLine >>= 
         (\size2 -> 
          putStrLn "What is cost of pizza 2" >>
          getLine >>= 
          (\cost2 -> 
            (\pizza1 -> 
                (\pizza2 -> 
                   (\betterPizza -> 
                        putStrLn (describePizza betterPizza)) (comparePizzas pizza1 pizza2))(read size2, read cost2)) (read size1, read cost1)))))


costData :: [ Double]
costData =  [18.0,16.0]

sizeData :: [Double]
sizeData =  [20.0,15.0]

maybeMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
maybeMain mx my mz mu = do
    size1 <- mx
    cost1 <- my
    size2 <- mz
    cost2 <- mu
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)
--
listMain :: [String]
listMain = do
    size <- sizeData
    cost <- costData
    let pizza = (size, cost)
    let betterPizza = comparePizzas pizza pizza 
    return (describePizza betterPizza)
