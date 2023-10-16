import qualified Data.Map as Map

data Box a = Box a deriving (Show)
data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 1.9 2.3 5.5

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "John" "Marc" "Antony"

type Initials = Triple Char

aInitial :: Initials
aInitial = Triple 'J' 'F' 'K'

first :: Triple a -> a
first (Triple x _ _) = x 

second :: Triple a -> a
second (Triple _ x _) = x 

third :: Triple a -> a
third (Triple _ _ x) = x 

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a 
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- 

data List a = Empty | Cons a (List a) deriving Show

ourMap :: (a -> b) -> List a -> List b
ourMap f Empty = Empty :: List a
ourMap f (Cons x xs) = Cons (f x) (ourMap f xs)

-- 

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--Q18.1
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b 
boxMap f (Box x) = Box (f x)

--Q18.2
oValues :: [Organ]
oValues = [Heart .. Spleen]
organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip oValues countOfOrgans)

totalOfOrgan :: [Organ] -> Organ -> Int
totalOfOrgan os organ = length lookedFor
    where lookedFor = filter (==organ) os

countOfOrgans :: [Int]
countOfOrgans = map (totalOfOrgan organs) oValues
