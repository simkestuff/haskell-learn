-- main :: IO ()
-- main = do
--     args <- mapM (\_ -> getLine) (take 3 [1 ..])
--     mapM_ putStrLn args
--
--
myReplicateM act n = mapM act (take n [1 .. ])

main :: IO ()
main = do
    args <- getContents
    let revArgs = reverse args 
    print revArgs
