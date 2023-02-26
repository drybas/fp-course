module Demo where

import Data.Char

{-- helper :: Int -> Int -> Int

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

factorial5 n | n >= 0 = helper 1 n
             | otherwise = 0

--helper2 :: Int -> Int -> (Int, Int)

helper2 1 f0 f1 = f0 + f1  
helper2 i f0 f1 = helper2 (i - 1) f1 (f0 + f1) 

--helper3 -1 f2 f1 = f2 - f1  
--helper3 i f2 f1 = helper3 (i + 1) f2 (f0 + f1) 

fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == -1 = 1
            | n > 1 = helper2 (n - 1) 0 1
            | n < -1 = (if odd(abs n) then 1 else (-1)) * helper2 (abs n - 1) 0 1
            | otherwise = error "failed to calculate"

fibonacciSample n = helper (0, 1) n where
    helper (r2, r1) n | n == 0 = r2
                      | n == 1 = r1
                      | n > 1  = helper (r1, r2 + r1) (n - 1) 
                      | n < 0  = helper (r1 - r2, r2) (n + 1) 

--fibHelper :: Int−>Int−>Int−>Int 
--fibHelper 0 val1 val 2 =   v a l 1f i b H e l p e r   1   v a l 1v a l 2=   v a l 2f i b H e l p e r   n   v a l 1v a l 2=f i b H e l p e r    ( n−1 )    v a l 2    ( v a l 1   +   v a l 2 )f i b: :I n t−>I n tf i b   n  =   f i b H e l p e r   n   0   1

fibonacci' :: (Num a1, Num a2, Ord a1) => a1 -> a2
fibonacci' n | n == 0 = 0
            | n == 1 = 1
            | n == -1 = 1
            | n > 1 = fibonacci' (n - 2) + fibonacci' (n - 1)  
            | n < -1 = fibonacci' (n + 2) - fibonacci' (n + 1) 

factorial6 n 
    | n >= 0 = let 
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
    in helper 1 n 
    | otherwise = error "arg must be >= 0"
--}
{-- seqA n | n == 0 = 1
       | n == 1 = 2
       | n >= 2 = let 
           helper _ _ ak2 2 = ak2
           helper ak ak1 ak2 n = helper ak1 ak2 (ak2 + ak1 - 2 * ak) (n - 1)
        in helper 1 2 3 n  --}

{-- sum'n'count n = (sum, count) where
    | n == 0 = (0, 1)
    | n > 0 = let 
        helper sum count 0 = (sum, count)
        helper sum count n = helper (sum + n `mod` 10) couunt + 1 n / 10
    in helper 0 1 n 
--}

sum'n'count' :: Int -> (Int, Int)
sum'n'count' n | n == 0 = (0, 1)
               | n /= 0 = helper 0 0 n where 
                    helper :: Int -> Int -> Int -> (Int, Int)
                    helper sum count n | n == 0 = (sum, count)
                                       | otherwise = helper (sum + n `mod` 10) (count + 1) (div n 10)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n | n == 0 = (0, 1)
              | n /= 0 = helper 0 0 (show (if n > 0 then n else (-1) * n)) where 
                    helper :: Integer -> Integer -> String -> (Integer, Integer)
                    helper sum count s | null s = (sum, count)
                                       | otherwise = helper (sum + toInteger(digitToInt(head s))) (count + 1) (tail s)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 999 f a where
    d = (b - a) / 1000
    helper sum n f left | n == 0 = d * (sum + (f a + f b) / 2)
                      | otherwise = let 
                          next = left + d 
                          in helper (sum + f next) (n - 1) f next 

main = do 
    putStrLn "Fibonacci "
    --let result = factorial5 5
    --let result = fibonacci (-1)
    -- let result = seqA 1
    -- print result 
