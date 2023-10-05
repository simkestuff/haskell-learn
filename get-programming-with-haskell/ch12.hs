import Language.Haskell.TH (Pat)
type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
data Patient = Patient { name :: Name 
                       , sex :: Sex
                       , age :: Age
                       , height :: Height
                       , weight :: Weight
                       , bloodType :: BloodType}
type Age = Int
type Height = Int
type Weight = Int
type PatientName = (FirstName, LastName)
data Sex = Male | Female
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType


johnDoe :: Patient 
johnDoe = Patient (Name "John" "Doe") Male 45 183 92 (BloodType AB Neg)

jackieSmith :: Patient
jackieSmith = Patient {
    name = NameWithMiddle "Jackie" "Djurdja" "Smith",
    sex = Female,
    age = 23,
    height = 165,
    bloodType = BloodType AB Neg,
    weight = 56 }


patientInfo :: PatientName -> Age -> Height -> Sex -> String
patientInfo patientName age height sex = name ++ " " ++ ageSexHeight
    where name = lastName patientName ++ ", " ++ firstName patientName
          ageSexHeight = "(" ++ show (sexInitial sex) ++ ", " ++ show age ++ " yrs. " ++ 
                         show height ++ " cm.)"

showName :: Name -> String
showName (Name fn ln) = ln ++ ", " ++ fn
showName (NameWithMiddle fn mn ln) = ln ++ ", " ++ mn ++ " " ++ fn

firstName :: PatientName -> FirstName
firstName (fn, _) = fn

lastName :: PatientName -> LastName
lastName (_, ln) = ln

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonate :: BloodType -> BloodType -> Bool
canDonate (BloodType O _) _ = True
canDonate _ (BloodType AB _) = True
canDonate (BloodType A _) (BloodType A _) = True
canDonate (BloodType B _) (BloodType B _) = True
canDonate _ _ = False


------------------

canDonateTo :: Patient -> Patient -> Bool 
canDonateTo (Patient _ _ _ _ _ (BloodType O _)) _ = True
canDonateTo _ (Patient _ _ _ _ _ (BloodType AB _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType A _)) (Patient _ _ _ _ _ (BloodType A _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType B _)) (Patient _ _ _ _ _ (BloodType B _)) = True
canDonateTo _ _ = False

-- ili canDonateTo p1 p2 = canDonate (bloodType p1) (bloodType p2)


patientSummary :: Patient -> String
patientSummary p = "************" ++
                 "Patient Name: " ++ showName (name p) ++ "\n" ++
                 "Sex: " ++ patientSex p ++
                 "Age" ++ show (age p) ++
                 "Height: " ++ show (height p) ++
                 "Weight: " ++ show (weight p) ++
                 "Blood Type: " ++ showBloodType (bloodType p)

patientSex :: Patient -> String
patientSex (Patient _ Male _ _ _ _) = "Male" 
patientSex (Patient _ Female _ _ _ _) = "Female" 
