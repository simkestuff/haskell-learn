import qualified Data.Map as Map
import Data.Maybe

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

lookupUsername :: GamerId -> Maybe UserName
lookupUsername gid = Map.lookup gid userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username 

creditsFromID :: GamerId -> Maybe PlayerCredits
-- creditsFromID gid = altLookupCredits (lookupUsername gid)
creditsFromID gid = lookupUsername gid >>= lookupCredits
-- creditsFromID gid = do
--     username <- lookupUsername gid 
--     lookupCredits username

echo = getLine >>= putStrLn

askName :: IO ()
askName = putStrLn "Koje ti je ime?"

nameStatement :: String -> String
nameStatement name = "Hello " ++ name ++ "!"

program :: IO ()
program = askName >> getLine >>= return . nameStatement >>= putStrLn


allFMapM :: Monad m => (a -> b) -> m a -> m b 
allFMapM f mx = mx >>= \x -> return (f x)

allApp :: Monad m => m (a -> b) -> m a -> m b 
allApp mf mx = mf >>= (\f -> mx >>= (\x -> return (f x)))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b 
bind Nothing f = Nothing
bind (Just x) f = f x
