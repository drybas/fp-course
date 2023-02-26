module Demo where

import Debug.Trace

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    succ (Odd x) = Odd(x + 2)
    pred (Odd x) = Odd(x - 2)
    fromEnum (Odd x) = fromIntegral x 
    toEnum x = (Odd $ fromIntegral x) 
    enumFrom (Odd x) = [Odd x] ++ enumFrom (Odd (x + 2)) --(enumFrom $ succ x)
    enumFromTo (Odd x) (Odd y)
        | x < y = Odd x : enumFromTo (succ (Odd x)) (Odd y)
        | y < x = [] 
        | otherwise = [Odd y]

    enumFromThen (Odd x) (Odd y) = Odd x : Odd y : helper (Odd (y + y - x)) (Odd (y - x))
        where
            helper (Odd y) (Odd d) = Odd y : helper (Odd(y + d)) (Odd d)

    enumFromThenTo (Odd x) (Odd y) (Odd n) 
            | x < n && x > y = []
            | x > n && x <= y = []
            | x > y && x <= n = [Odd x]
            | otherwise = Odd x : Odd y : helper (Odd (y + y - x)) (Odd (y - x)) (Odd n) (if x <= y then 1 else -1)
        where helper (Odd y) (Odd d) (Odd n) s 
                | s > 0 && y <= n = Odd y : helper (Odd(y + d)) (Odd d) (Odd n) s 
                | s < 0 && y >= n = Odd y : helper (Odd(y + d)) (Odd d) (Odd n) s 
                | otherwise = []

{-- instance Enum Odd where
  toEnum i = Odd(toInteger i)
  fromEnum (Odd n) = fromEnum n

  succ (Odd n) = Odd (n+2)
  pred (Odd n) = Odd (n-2)

  enumFrom (Odd n) = map Odd [n,n+2..]
  enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
  enumFromThen (Odd n) (Odd n') = map Odd [n,n'..]
  enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n'..m]
  --}
-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)

-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)
--}
testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

{--
    1. первый элемент -> вычесть
    2. 
    3. 
--}

         {--| otherwise = [ y : map (concat [i]) (change (x - i)) | i <- coins, x > i, y <- [[]] ]
         --}
coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
{--
change x | x == 0 = []
         | otherwise = [ y : map (i:) (change(x - i)) | i <- coins, x > i, y <- [[]] ]

change' :: (Ord a, Num a) => a -> [[a]]
--}

{-- change x | x == 0 = [] --}
{-- change' x | x < minimum coins = []
          | x == 0 = []
          | otherwise = [ i | i <- coins, x > i, y <- ]

        change' x = [[i] | i <- coins, i == x]
    c = [ i | i <- coins, x > i]
    --}

change x = [y | y <- change' x, x == sum y]
    where change' x | x == 0 = [[]]
                    | x < minimum coins = [[]]
                    | otherwise = let c = [ i | i <- coins, x >= i]
            in [ i : y | i <- c, y <- change(x - i)] 

        {-- change' x | x < minimum coins = [] 
        change' x = [[i] | i <- coins, i == x]
        change' x = [[]] --}
{--    change' x | x == 0 = []
          | otherwise = [ y | i <- coins, x > i, y <- (change'(x - i)) ]
--} 