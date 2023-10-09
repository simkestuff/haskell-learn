data SixSideDie = S1 | S2| S3| S4| S5| S6 deriving (Eq, Ord, Enum)

instance Show SixSideDie where
   show :: SixSideDie -> String
   show S1 = "one" 
   show S2 = "two" 
   show S3 = "three" 
   show S4 = "four" 
   show S5 = "five" 
   show S6 = "six" 

-- instance Eq SixSideDie where
--     (==) :: SixSideDie -> SixSideDie -> Bool
--     (==) S1 S1 = True
--     (==) S2 S2 = True
--     (==) S3 S3 = True
--     (==) S4 S4 = True
--     (==) S5 S5 = True
--     (==) S6 S6 = True
--     (==) _ _   = False
--
-- instance Ord SixSideDie where
--     compare :: SixSideDie -> SixSideDie -> Ordering
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ
--     compare S4 _ = GT
--     compare _ S4 = LT
--     compare S3 S3 = EQ
--     compare S3 _ = GT
--     compare _ S3 = LT
--     compare S2 S2 = EQ
--     compare S2 _ = GT
--     compare _ S2 = LT
--     compare S1 S1 = EQ
--
data Name = Name (String,String) deriving (Eq, Show)

names :: [Name]
names = [ Name ("Emil","Cioran")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche")]

instance Ord Name where
    compare :: Name -> Name -> Ordering
    compare (Name (fn1, ln1)) (Name (fn2, ln2)) = compare (ln1, fn1)  (ln2, fn2)
    -- compare (Name (fn1, ln1)) (Name (fn2, ln2))
    --     | ln1 /= ln2 = compare ln1 ln2
    --     | otherwise  = compare fn1 fn2

-- Q14.2
data FiveSideDie = F1 | F2 | F3 | F4 | F5 deriving (Ord, Eq, Enum, Show)

class Die a where
   points :: a -> Int

instance Die FiveSideDie where
    points :: FiveSideDie -> Int
    points die =  fromEnum die + 1

