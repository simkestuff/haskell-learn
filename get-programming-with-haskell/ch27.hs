import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)

data RobotPart = RobotPart {
     name :: String 
    ,description :: String
    ,cost :: Double
    ,count :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { 
     name = "left arm"
    ,description = "left arm for face punching!"
    ,cost = 1000.00
    ,count = 3
}
rightArm :: RobotPart
rightArm = RobotPart {
     name = "right arm"
    ,description = "right arm for kind hand gestures"
    ,cost = 1025.00
    ,count = 5
}
robotHead :: RobotPart
robotHead = RobotPart {
     name = "robot head"
    ,description = "this head looks mad"
    ,cost = 5092.25
    ,count = 2
}

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName, "</h2>"
                           ,"<p><h3>desc</h3>",partDesc
                           ,"</p><p><h3>cost</h3>"
                           ,partCost
                           ,"</p><p><h3>count</h3>"
                           ,partCount,"</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)


partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList [(1, robotHead), (2, leftArm), (3, rightArm)]

insertSnippet :: Maybe Html -> IO ()
insertSnippet = undefined

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = Map.elems partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm


-----------------------------------------------------

data Box a = Box a deriving (Show)

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box x) = Box (f x)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = take n . repeat <$> box

myBox :: Box Int
myBox = Box 1

unwrapped :: Box a -> a
unwrapped (Box x) = x 

------------------------------------------------------

partCost :: Int -> Map.Map Int RobotPart -> Maybe Double
partCost k db = cost <$> Map.lookup k db

getCost :: Int -> Maybe Double
getCost = flip partCost partsDB

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Nema toga"
printCost (Just aCost) = print aCost


-- main :: IO ()
-- main = do
--     input <- getLine
--     let partID = read input
--     let result = getCost partID
--     -- let aCost = if isNothing result
--     --             then "Ne postoji"
--     --             else (show . fromJust) result
--     -- putStrLn aCost
--     printCost result

-------------------------------------------------------- -
--
cheaperPart :: RobotPart -> RobotPart -> Double
cheaperPart rp1 rp2 = min (cost rp1) (cost rp2)

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    id1 <- readInt
    id2 <- readInt
    let rp1 = Map.lookup id1 partsDB
    let rp2 = Map.lookup id2 partsDB
    let lowerCost = cheaperPart <$> rp1 <*> rp2 
    printCost lowerCost

