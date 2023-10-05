class Describable a where
    describe :: a -> String

inc :: Int -> Int
inc n = n + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
