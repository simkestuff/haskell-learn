data User = User { name :: String
                 , gamerId :: Int
                 , score :: Int } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

poorShmack :: Maybe User
poorShmack = User <$> serverUsername <*> serverGamerId <*> serverScore

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    input <- User <$> getLine <*> readInt <*> readInt
    print input
