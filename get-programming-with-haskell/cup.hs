import Data.Binary.Get (isEmpty)
cup fl10z = \message -> message fl10z

getOz aCup = aCup (\fl10z -> fl10z)

drink aCup ozDrank = cup newOz
    where newOz = max (getOz aCup - ozDrank) 0

isEmpty aCup = getOz aCup == 0
