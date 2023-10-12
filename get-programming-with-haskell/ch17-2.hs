data PTable = PTable Events Probs
newtype Events = Events [String] deriving (Show)
newtype Probs = Probs [Double] deriving(Show)

instance Semigroup Events where
    (<>) :: Events -> Events -> Events
    Events [] <> events = events
    events <> Events [] = events
    Events xs <> Events ys = Events (combineEvents xs ys)

instance Monoid Events where
    mempty = Events []
    mappend = (<>)

instance Semigroup Probs where
    Probs [] <> probs = probs
    probs <> Probs [] = probs
    Probs xs <> Probs ys = Probs (combineProbs xs ys)

instance Monoid Probs where
    mempty = Probs []
    mappend = (<>)



cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys = zipWith f repeatedXs cycledYs
    where repeatedXs = mconcat (map (take timesRepeated . repeat ) xs)
          cycledYs =  cycle ys
          timesRepeated = length ys

combineEvents :: [String] -> [String] -> [String]
combineEvents = cartCombine (\e1 e2 -> mconcat [e1, "-", e2])

combineProbs :: [Double] -> [Double] -> [Double]
combineProbs = cartCombine (*)


createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs) 
    where totalProbs = sum probs
          normalizedProbs = map (/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show :: PTable -> String
    show (PTable (Events events) (Probs probs)) = mconcat pairs 
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (PTable (Events []) (Probs [])) <> ptable = ptable
    ptable <> (PTable (Events []) (Probs [])) = ptable
    (PTable e1 p1) <> (PTable e2 p2) = createPTable (e1 <> e2) (p1 <> p2)

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])
    mappend = (<>)

