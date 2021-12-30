module Demo where 
import Data.List

lastElem :: [a] -> a
lastElem = foldl1 (\_ y -> y)

lastElem' :: [a] -> a
lastElem' = foldl1 seq

revRange :: (Char,Char) -> [Char]
revRange (b, e) = unfoldr g e 
  where g n = if b <= n then Just(n, pred n) else Nothing 

revRange' :: (Char,Char) -> [Char]
revRange' = unfoldr g
  where g (b, e) = if b <= e then Just(e, (b, pred e)) else Nothing 