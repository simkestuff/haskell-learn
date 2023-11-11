doorPrize :: [Int]
doorPrize = [1000,2000,3000]

-- boxPrize :: Num a => [a -> a]
boxPrize :: [Int]
boxPrize = [10,50]

totalPrizes :: [Int]
totalPrizes = pure (*) <*> doorPrize <*> boxPrize

primesToN :: Int -> [Int]
primesToN n = filter isNotComposite [ 2..n ]
    where compositeNumbers = (*) <$>  [ 2..n ] <*> [ 2..n ]
          isNotComposite = not . ( `elem` compositeNumbers)

allFmap :: Applicative f => (a -> b) -> f a -> f b 
allFmap g ax = pure g <*> ax

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

beerBought :: [Int]
beerBought = [6, 12]

beerGone :: Int 
beerGone = 4 

guests :: [Int]
guests = [2,3]

averageBeers :: [Int]
averageBeers = [3, 4]

quantity :: [Int]
quantity = pure (-) <*> beerConsumptions <*> availableBeer
    where availableBeer = pure (-) <*> beerBought <*> [beerGone]
          drinkers = pure (+) <*> guests <*> [2]
          beerConsumptions = pure (*) <*> drinkers <*> averageBeers

startingBeer :: [Int]
startingBeer = [6,12]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> startingBeer

totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

beersPerGuest :: [Int]
beersPerGuest = [3,4]

totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*> beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = (pure (-)) <*> totalBeersNeeded <*> remainingBeer
