import qualified Data.Map as Map 

type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]


toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
    where rlat = toRadians lat
          rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1,rlong1) = latLongToRads coords1
          (rlat2,rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

main :: IO ()
main = do
    putStrLn "grad 1:"
    startInput <- getLine
    let startCity = Map.lookup startInput locationDB
    putStrLn "grad 2:"
    endInput <- getLine
    let endCity = Map.lookup endInput locationDB
    let distance = haversine <$> startCity <*> endCity
    printDistance distance

    
haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO ioval1 ioval2 = do
--    val1 <- ioval1 
--    val2 <- ioval2 
--    let h = haversine val1 val2
--    return h
haversineIO ioval1 ioval2 = haversine <$> ioval1 <*> ioval2
