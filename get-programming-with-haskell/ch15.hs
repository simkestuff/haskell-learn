data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Enum, Bounded, Show)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN n c = toEnum rotation
    where halfAlphabet = n `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` n
          
largestCharNum :: Int
largestCharNum = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = largestCharNum + 1

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where rot4l = rotN alphaSize
          alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
