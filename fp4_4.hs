import Data.Char(isDigit)
import Data.Maybe (fromMaybe)
import System.Directory.Internal.Prelude (fromMaybe)

data Coord a = Coord a a
    deriving(Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-- distance (Coord x1 y1) (Coord x2 y2) = sqrt $ ((+) `on` (^2)) (x2 - x1) (y2 - y1)  

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = (+) (abs (x1 - x2)) $ abs (y1 - y2)

getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord xi yi) = Coord (s * fromIntegral xi + s / 2) (s * fromIntegral yi + s / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell s (Coord x y) = Coord (floor(x / s)) (floor(y / s))


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | not . isDigit $ x = findDigit xs
                 | otherwise = Just x

findDigitOrX :: [Char] -> Char
findDigitOrX a = case findDigit a of
                Just y -> y
                Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList Nothing = [] 
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe _ = Nothing 

