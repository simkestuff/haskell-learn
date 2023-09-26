import Data.List

ifEven myFunc n = if even n 
                  then myFunc n 
                  else n 

double x = x*2

ifEvenDouble = ifEven double

names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Albert","Sumner"),
         ("Nedjeljko","Sumner"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else if firstName1 > firstName2
                                         then GT
                                         else if firstName1 < firstName2
                                              then LT
                                              else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


compareLastNames2 name1 name2 = case lastNameCompare of
    EQ -> firstNameCompare
    _  -> lastNameCompare
    where firstNameCompare = compare (fst name1) (fst name2)
          lastNameCompare = compare (snd name1) (snd name2)


compareLastNames3 name1 name2 = let firstNameCompare = compare (fst name1) (fst name2)
                                    lastNameCompare = compare (snd name1) (snd name2)
                                in
                                  case lastNameCompare of
                                    EQ -> firstNameCompare
                                    _  -> lastNameCompare

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "w" -> wOffice
    _    -> (\name -> fst name ++ " " ++ snd name)

sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

wOffice name = let nameText = (fst name) ++ " " ++ (snd name) 
               in
                 nameText ++  " Esq" ++ " - PO BOX 999 - Washington, WA 666"

addressLetter name location = getLocationFunction location name
