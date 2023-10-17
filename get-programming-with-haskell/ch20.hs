import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]
file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]
file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]
file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a] 

instance Show a => Show (TS a) where
    show (TS times values) = mconcat (intersperse "\n" (map showTVPair pairs))
        where pairs = zip times values

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

createTS :: [Int] -> [a] -> TS a
createTS times values = TS timeRange valuesInRange
    where timeRange = [minimum times .. maximum times]
          valuesMap = Map.fromList (zip times values)
          valuesInRange = map ( `Map.lookup` valuesMap) timeRange

fileToTS :: [(Int, a)] -> TS a
fileToTS file = createTS times values
    where (times, values) = unzip file

showTVPair :: Show a => (Int, Maybe a) -> String
showTVPair (i, Nothing) = show i ++ ": " ++ "NA"
showTVPair (i, Just x) = show i ++ ": " ++ show x

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair m (key, Nothing) = m
insertMaybePair m (key, Just x) = Map.insert key x m

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts = ts
combineTS ts (TS [] []) = ts
combineTS (TS times1 values1) (TS times2 values2) = TS timesRange valuesInRange
    where mapStart = foldl insertMaybePair Map.empty (zip times1 values1)
          mapEnd = foldl insertMaybePair mapStart (zip times2 values2)
          timeMin = min (minimum times1) (minimum times2)
          timeMax = max (maximum times1) (maximum times2)
          timesRange = [timeMin .. timeMax]
          valuesInRange = map (`Map.lookup` mapEnd) timesRange


tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-------------------------------------------

mean :: Real a => [a] -> Double
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: TS Double -> Maybe Double
meanTS (TS [] []) = Nothing
meanTS (TS times values) 
    | all isNothing values = Nothing
    | otherwise = Just ((mean . catMaybes) values)

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
          newFunc (i1, Nothing) (i2, v2) = (i2, v2)
          newFunc (i1, v1) (i2, Nothing) = (i1, v1)
          newFunc (i1, Just v1) (i2, Just v2) = if func v1 v2 == v1 
                                      then (i1, Just v1)
                                      else (i2, Just v2)

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times vals)
    | all isNothing vals = Nothing
    | otherwise = Just (foldr newFunc (0, Nothing) pairs)
        where pairs = zip times vals
              newFunc = makeTSCompare func

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min


diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair (Just x) (Just y) = Just (x - y)
diffPair _ _ = Nothing

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS ids values) = TS ids diffValues
    where diffValues = Nothing : zipWith diffPair shiftValues values
          shiftValues = tail values

movingAverage :: Real a => TS a -> Int -> TS Double
movingAverage = undefined
(dosmth . take 3) vals : (func (drop 3 vals))

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe [] = Nothing
meanMaybe vals 
    | any isNothing vals = Nothing
    | otherwise = Just (mean (catMaybes vals))

