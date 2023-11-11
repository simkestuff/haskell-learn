import qualified Data.Map as Map

data Grade = F | D | C | B | A deriving (Show, Ord, Eq, Enum, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate { 
    candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [passedCoding ,passedCultureFit ,educationMin]

testCandidateViable = Candidate {candidateId=23, 
                                 codeReview=A, 
                                 cultureFit=B, education=PhD}

testCandidateNotViable = Candidate {candidateId=33, 
                                 codeReview=B, 
                                 cultureFit=B, education=MS}

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                        , codeReview = A
                        , cultureFit = A
                        , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                        , codeReview = C
                        , cultureFit = A
                        , education = PhD }


candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                        , codeReview = A
                        , cultureFit = B
                        , education = MS }


candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1), 
                            (2,candidate2), 
                            (3,candidate3), 
                            (4,testCandidateViable), 
                            (5,testCandidateNotViable)]


readInt :: IO Int
readInt = putStrLn "Enter ID:" >> getLine >>= return . read

readGrade :: IO Grade
readGrade = putStr "Enter grade:" >> getLine >>= return . read

readDegree :: IO Degree
readDegree = putStr "Enter degree:" >> getLine >>= return . read

readCandidate :: IO Candidate
readCandidate = do
    cid <- readInt
    codeGrade <- readGrade
    reviewGrade <- readGrade
    educ <- readDegree
    return (Candidate cid codeGrade reviewGrade educ)

assesCandidateIO :: IO String
assesCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate 
    let result = if passed 
                 then "passed"
                 else "failed"
    return result


assesCandidateMaybe :: Int -> Maybe String
assesCandidateMaybe cid = do
    candidate <- Map.lookup cid candidateDB
    let passed = viable candidate 
    let statement = if passed 
                    then "passed"
                    else "failed"
    return statement

candidates :: [Candidate]
candidates = [candidate1
            ,candidate2
            ,candidate3]


assesCandidateList :: [Candidate] -> [String]
assesCandidateList cs = do
    candidate <- cs 
    let passed = viable candidate 
    let statement = if passed 
                    then "passed"
                    else "failed"
    return statement



assesCandidate :: Monad m => m Candidate -> m String
assesCandidate candidates = do
    candidate <- candidates 
    let passed = viable candidate 
    let statement = if passed 
                    then "passed"
                    else "failed"
    return statement
