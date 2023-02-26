module Demo where 

import Data.Char (isDigit, isLower, isUpper)

readDigits :: String -> (String, String)
readDigits = span isDigit 

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p0 p1 xs = filter (\x -> p0 x || p1 x) xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
--qsort [x] = [x]
qsort (x:xs) = 
    let l = filter (<x) xs
        r = filter (>x) xs
    in 
        qsort l ++ [x] ++ qsort r 

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes xs = concatMap (\x-> [x ^2, x ^ 3]) xs

{-- def perms_1(A):
    if not A:
        return [[]]
    perms = []
    for pi in perms_1(A[1:]):
        for i in range(len(A)):
            perms.append(pi[:i] + [A[0]] + pi[i:])
    return perms --}

{-- perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = 
        let ys = perms xs 
        in 
            concatMap(\y-> [y,x]) ys  --}

delAllUpper :: String -> String
delAllUpper = unwords . filter (\x -> any isLower x) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max z $ max x y)

fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

repeatHelper = flip const 42 
repeatA = iterate repeatHelper
