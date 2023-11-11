import Control.Monad (guard)
import Control.Applicative (Alternative)

data Name = Name { firstName :: String
                  ,secondName :: String}

instance Show Name where
    show :: Name -> String
    show (Name fn ln) = mconcat [fn, " ", ln]

data GradeLevel = Freshman 
                | Sophmore
                | Junior
                | Senior     deriving (Show, Eq, Ord)

data Student = Student { studentId :: Int
                        ,gradeLevel :: GradeLevel
                        ,studentName :: Name } deriving Show

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))]


_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals 
    return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals 
    guard (test val)
    return val

startsWith :: Char -> String -> Bool
startsWith _ [] = False
startsWith c (x:xs) = c == x

data Teacher = Teacher { teacherId :: Int
                        ,teacherName :: Name } deriving Show

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
            ,Teacher 200 (Name "Susan" "Sontag")]

data Course = Course { courseId :: Int
                      ,courseTitle :: String
                      ,teacher :: Int } deriving Show

courses :: [Course]
courses = [Course 101 "French" 100
           ,Course 201 "English" 200]

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join valsA valsB propA propB = do
    valA <- valsA 
    valB <- valsB
    let dPairs = (valA, valB)
    guard ((propA . fst) dPairs == (propB . snd) dPairs)
    return dPairs
    

joinData = _join teachers courses teacherId teacher 
whereResult = _where ((== "English"). courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery = (\joinData -> 
                                           (\whereResult -> selectQuery whereResult) 
                                             (whereQuery joinData)) joinQuery
                                          

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) |
                  HINQ_ (m a -> m b) (m a)


runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b 
runHINQ (HINQ selectQuery joinQuery whereQuery) = _hinq selectQuery joinQuery whereQuery
runHINQ (HINQ_ selectQuery joinQuery) = _hinq selectQuery joinQuery (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name  
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((=="English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

