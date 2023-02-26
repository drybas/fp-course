module Demo where 

concatList :: [[a]] -> [a]
concatList = foldr (++) [] 


lengthList :: [a] -> Int
lengthList = foldr (\_ s -> s + 1) 0 

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if (x `mod` 2) > 0 then s + x else s) 0

sumOdd' :: [Integer] -> Integer
sumOdd' xs = foldr (+) 0 $ filter odd xs

meanList :: [Double] -> Double
meanList = (\(c, s)-> s / c) . foldr (\x (c,s) -> (c + 1, s + x)) (0,0) 

evenOnly :: [a] -> [a]
evenOnly = foldr (\(a,b) ys -> if even a then b:ys else ys) [] . zip [1..] 
--evenOnly = foldr (uncurry ($)) [] . zip (cycle [const id, (:)]) 