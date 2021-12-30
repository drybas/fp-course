module Demo where 

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1)  (Point x2 y2) = distanceToOrigin(Point(x2 - x1) (y2 - y1))


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle x) = pi * x ^ 2
area (Rectangle x1 x2) = x1 * x2

area' :: Shape -> Double
area' x = case x of  
           (Circle x0) -> pi * x0 ^ 2
           (Rectangle x1 x2) -> x1 * x2

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x1 x2) = x1 == x2
isSquare _ = False


{--  --}
{-- data Result = Success | Fail
doSomeWork :: SomeData -> (Result,Int)

data Result' = MySuccess | MyFail Int

instance Show Result' where
    show x = case x of 
        (MyFail e) -> "Fail: " ++ show e
        MySuccess -> "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' = g . doSomeWork where
    g = \(r, c) -> case r of 
        Success -> MySuccess
        Fail -> MyFail c
--}



