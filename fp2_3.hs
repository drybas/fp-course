a = 12
b = 7.22
c = 4.12
d = 0.12

ip = show a ++ show b ++ show c ++ show d 


class (Bounded a, Eq a, Enum a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x | maxBound == x = minBound
            | otherwise = succ x

    spred :: a -> a
    spred x | minBound == x = maxBound 
            | otherwise = pred x

avg :: Int -> Int -> Int -> Double
--avg a b c = realToFrac(a + b + c) / 3.0
--avg a b c = fromIntegral(toInteger a + toInteger b + toInteger c) / 3.0

avg a b c = fromInteger x / 3.0 where 
    x :: Integer
    x = toInteger a + toInteger b + toInteger c

--avg :: Int -> Int -> Int -> Double
--avg a b c = (sum $ map fromIntegral [a,b,c]) / 3