module Demo where 
import Data.Foldable

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f a []     = a
foldl'' f a (x:xs) = let a' = f a x in seq a' (foldl'' f a' xs)

foo = foldl (+) 0 [1..100000000]
foo' = foldl'' (+) 0 [1..100000000] 

enumFromTo' n m = if n < m then n : enumFromTo' (n+1) m else []

mySum acc 0 = acc
mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1

goSum = mySum (0, ())