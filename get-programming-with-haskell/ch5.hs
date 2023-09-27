-- qc 5.1
genIfXEven x = \f -> if even x then f x else x

double x = x * 2

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

getHostRequestBuilder host = \apiKey resource id -> getRequestURL host apiKey resource id

exampleUrlBuilder = getHostRequestBuilder "https://example.com"

genApiRequestBuilder hostBuilder apiKey = \resource id -> hostBuilder apiKey resource id 

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "moj-tokoen"

genResourceRequestBuilder hostAndApiBuilder resource = \id -> hostAndApiBuilder resource id

myExmapleBookUrlBilder = genResourceRequestBuilder myExampleUrlBuilder "book"

-- qc5.3 
myBookUrl = getRequestURL "https://example.com" "neki-moj-token" "book"
-- exampleUrlBuilder "neki-moj-token" "book"
--
-- qc 5.3
-- subtract 2 8 -> 6 
-- subtract2 x = subtract 2 x
subtract2  =  flip (-) 2 


-- q5.1
ifEven myFunc n = if even n 
                  then myFunc n 
                  else n 

ifEvenInc = ifEven inc 
ifEvenDouble = ifEven double 
ifEvenSquare = ifEven square 

inc x = x + 1 
square x = x * x

--q5.2
binaryPartialApplication f x  = \y -> f x y


