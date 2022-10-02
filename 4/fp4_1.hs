module Demo where

data Color = Red | Green | Blue deriving(Read)

instance Show Color where 
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

stringToColor :: String -> Color
stringToColor = read

charToInt :: Char -> Int
charToInt x = g . lookup x $ xs where 
    xs = zip ['0'..] [0..9]
    g = \i -> case i of 
        Just y  -> y

data LogLevel = Error | Warning | Info

cmp Error Error = EQ
cmp Error _ = GT 
cmp _ Error = LT 
cmp Warning Warning = EQ
cmp Info Warning = LT
cmp Warning Info = GT
cmp Info Info = EQ 

{-- 
instance Enum LogLevel where
  fromEnum Error    = 3
  fromEnum Warning  = 2
  fromEnum Info     = 1
    
cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (fromEnum x) (fromEnum y) 
--}

{-- 
data Result = Fail | Success
doSomeWork :: SomeData -> (Result,Int)

processData :: SomeData -> String
processData = g . doSomeWork where
    g = \(r, c) -> case r of 
        Success -> "Success"
        Fail -> "Fail: " ++ show c
--}