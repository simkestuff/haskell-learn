xorBool :: Bool -> Bool -> Bool
xorBool b1 b2 = b1 /= b2

xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' x = if reminder == 0 
               then False : intToBits' reducedX
               else True : intToBits' reducedX
    where reminder = x `mod` 2
          reducedX = x `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound )

intToBits :: Int -> Bits
intToBits x = falsePadding ++ reversedBits
    where falsePadding = take howManyFalsePaddings (cycle [False])
          howManyFalsePaddings = maxBits - length reversedBits
          reversedBits = reverse (intToBits' x)

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int 
bitsToInt lob = sum (map (\(b,i) -> if b 
                                    then 2^i
                                    else 0) 
                         (zip lob [size-1,size-2 .. 0]))
    where size = length lob

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plain = map (\(b1,b2) -> xor b1 b2) (zip convertedPad convertedPlain)
    where convertedPad = map charToBits pad
          convertedPlain = map charToBits plain

applyOTP :: String -> String -> String
applyOTP pad plain = map bitsToChar lob 
    where lob = applyOTP' pad plain

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode :: OneTimePad -> String -> String
    encode (OTP pad) text = applyOTP pad text

    decode :: OneTimePad -> String -> String
    decode = encode

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
