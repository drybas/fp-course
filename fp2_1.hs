module Demo where

doItYourself = f . g . h

f = logBase 2 

g  = (^3) 

h = max 42

  --ssucc x | (maxBound :: a) == x = minBound :: a
  --a | otherwise = succ x
    --spred :: a -> a
    --spred x | maxBound :: a 
  --spred x | (minBound :: a) == x = maxBound :: a 
  -- dfdf      | otherwise = pred 