module Demo2 where

import Data.Function

getSecondForm :: t -> t1 -> t2 -> t1

getSecondForm x y z = y

f :: a -> a -> b -> a -> a
f x y z j = x
f x y z j = y
f x y z j = j

fun :: a -> (a,b) -> a -> (b,a,a)
fun xa z xb = (snd z, xa, xa) 
fun xa z xb = (snd z, xa, xb) 
fun xa z xb = (snd z, xb, xa) 

fun xa z xb = (snd z, , xa) 
fun xa z xb = (snd z, xa, xa) 
fun xa z xb = (snd z, xa, xa) 




sumSquares = (+) `on` (^2)

multSecond = g `on` h

--g x y = x * y 

g = (*)
h t = snd t

--h (x, y) = y 


--getSecondFrom True 'x' "Hello"

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z) 
sum3squares = (\x y z -> x+y+z) `on3` (^2)

--sum3squares 1 2 3

doItYourself = f . g . h

f = (logBase 2)

g = (^3) 

h = if x > 42 then x else 42

instance  Eq Bool