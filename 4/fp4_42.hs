eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data Coord a = Coord a !a

getX :: Coord a -> a
getX (Coord x _) = x

getY :: Coord a -> a
getY (Coord _ y) = y

{--
Maybe -> Int  -
Int -> Int       *
Maybe Int -> Int *
Nothing          -
Either (Int -> Int) Maybe - 
Either True False         - 
Either (Int -> (,)) Int   -
(Maybe Int, Either (Int -> (Char, Char)) Int) *
Maybe (Int -> Either Int Int) *
--}