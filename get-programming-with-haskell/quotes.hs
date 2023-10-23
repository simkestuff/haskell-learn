sampleInputs = "1\n2\nn\n"

quotes :: [String] -> [String]
quotes [] = []
quotes ("n": xs) = []
quotes (n:xs) = userChoice n : quotes xs

userChoice :: String -> String
userChoice n = "kvota " ++ n

main :: IO ()
main = do
    userInputs <- getContents
    mapM_ print (quotes (lines userInputs))


-- quotes :: [String]
-- quotes = ["quote 1"
--     ,"quote 2"
--     ,"quote 3"
--     ,"quote 4"
--     ,"quote 5"]
--
-- lookupQuote :: [String] -> [String]
-- lookupQuote [] = []
-- lookupQuote ("n":xs) = []
-- lookupQuote (x:xs) = quote : (lookupQuote xs)
--     where quote = quotes !! (read x - 1)
--
-- main :: IO ()
-- main = do
--     userInput <- getContents
--     mapM_ putStrLn (lookupQuote (lines userInput))
