import Control.Monad

numbersAndSquares :: [(Int, Int)]
numbersAndSquares = do
    num <- [ 1..10 ]
    return (num, num ^2)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fun xs = do
    x <- xs 
    guard (fun x)
    return x


mm :: [(Int, Int)]
mm = [(1,31), (2,28), (3,31), (4,30), (5,31), (6,30), (7,31), (8,31), (9,30), (10,31), (11,30), (12,31)]
           
dates :: [String]
dates = [mconcat [day, ".", month] | m <- mm,
                                     let month = (show . fst) m,
                                     let dd = snd m,
                                     d <- [1 .. dd],
                                     let day = show d]

dates2 :: [String]
dates2 = do
    m <- mm
    let month = (show . fst) m 
    let dd = snd m 
    d <- [1 .. dd]
    let day = show d 
    let result = mconcat [day, ".", month]
    return result

dates3 :: [String]
dates3 = mm >>= 
          (\m ->
           (\month -> 
             (\dd ->
               [1 .. dd] >>= 
                 (\d ->
                    (\day -> 
                       (\result -> 
                          return result) (mconcat [day, ".", month])) (show d)))(snd m)) ((show . fst) m))
