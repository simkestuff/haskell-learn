myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs 
    where totalProbs = sum probs
          normalizedProbs = map (/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show :: PTable -> String
    show (PTable events probs) = mconcat pairs 
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (PTable [] []) <> ptable = ptable
    ptable <> (PTable [] []) = ptable
    (PTable e1 p1) <> (PTable e2 p2) = createPTable (combineEvents e1 e2) (combineProbs p1 p2)

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys = zipWith f repeatedXs cycledYs
    where repeatedXs = mconcat (map (take timesRepeated . repeat ) xs)
          cycledYs =  cycle ys
          timesRepeated = length ys

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine (\e1 e2 -> mconcat [e1, "-", e2])

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)


data Color = Red |
    Yellow |
    Blue |
    Green |
    Purple |
    Orange |
    Brown |
    Prozirno deriving (Show,Eq)

instance Semigroup Color where
    (<>) Prozirno color = color
    (<>) color Prozirno = color
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown

instance Monoid Color where
    mempty :: Color
    mempty = Prozirno

    mappend = (<>)
