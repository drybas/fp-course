module Demo where

import Control.Arrow
import Data.List (transpose, span)
--import GHC.List (span)

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst

--let ls = [1, 2, 3, 4, 5]

nTimes :: a -> Int -> [a]
nTimes' x n
  | n == 0 = []
  | otherwise = x : nTimes' x (n - 1)

nTimes x n = helper x n [] where
    helper x n lst | n == 0 = lst
                   | n > 0 = helper x (n - 1) $ x : lst

sndHead = snd . head
--sndHead ((,) y z : x) = x -- tail 
--sndHead ((:) ((,) _ x) y) = x -- err
--sndHead ((,) x y : z) = x  -- first
--sndHead ((,) y x : z) = x -- +
--sndHead ((,) ((:) _ _) x) = x -- failed
-- sndHead ((_, x) : _) = x -- +
--sndHead ((,) y z : x) = x

oddsOnly :: Integral a => [a] -> [a]
--oddsOnly xs = helper xs [] where 
--    helper xs ys | null xs = ys 
--                 | odd(head xs) = helper (tail xs) $ ys : (head xs) 
--                 | otherwise = helper (tail xs) ys


oddsOnly xs 
    | null xs = []
    | odd(head xs) = head xs : oddsOnly (tail xs) 
    | otherwise = oddsOnly (tail xs)

oddsOnly' [] = []
oddsOnly' (x:xs) | odd x = x : oddsOnly' xs
                | otherwise = oddsOnly' xs


isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = uncurry (==) . (id &&& reverse)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == sx where 
    sx = reverse xs

--oddsOnly xs = helper xs [] where 
--    helper xs ys | null xs = ys 
--                 | odd(head xs) = helper (tail xs) $ ys : (head xs) 
--                 | otherwise = helper (tail xs) ys

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3' [] [] [] = []
sum3' (a:as) (b:bs) (c:cs) = a + b + c : sum3' as bs cs
sum3' a b c = sum3' (f a) (f b) (f c) where 
    f x | null x = [0]
        | otherwise = x

sum3 as bs cs = helper (transpose [as, bs, cs]) where 
    helper (a:xs) = sum a : helper xs
    helper [] = []

groupElems :: Eq a => [a] -> [[a]]
{--
attempt 1:
this function groups all elements without any condition
E.g.: [1,2,3,2,4] -> [[1],[2,2],[3],[4]]
--}
groupElemsAll[] = []
groupElemsAll(x:xs) = 
    let t = find x xs where
        find a [] = (,) [a] []
        find a (x:xs) | a == x = (,) (x : fst(find a xs)) (snd(find a xs))
                      | otherwise = (,) (fst(find a xs)) (x : snd(find a xs))
    in 
        fst t : groupElemsAll (snd t)


{-- 
attempt 2
    this function groups all elements only if they appear subsequentailly  
    E.g.: [1,2,3,2,4] -> [[1],[2,2],[3],[4]
--}

groupElems' [] = []
groupElems' (x:xs) = find xs [[x]] where
        find:: Eq a => [a] -> [[a]] -> [[a]]
        find [] ys = ys
        find (x:xs) ((y':ys'):ys) | y' /= x = find xs [[x]] ++ ((y':ys'):ys)
                                  | y' == x = find xs (((y':ys')++[x]):ys) 
--}

groupElems [] = []
groupElems (x:xs) =
    let t = span (==x) xs 
    in
       (x : fst t) : groupElems (snd t)
