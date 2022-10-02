
import Data.List

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

instance Show Bit where
    show x = case x of 
            Zero -> "0"
            One -> "1"

instance Show Z where
    show (Z s b) = case s of
                Minus -> "Minus " ++ show b
                Plus -> "Plus " ++ show b

convert b = foldl (\acc (x, y) -> case x of 
                One -> acc + 2 ^ y 
                _ -> acc) 0 (zip b [0..])

convert' (Z s b) = case s of
                Minus -> -1 * convert b
                Plus -> convert b

dissect :: (Num a, Ord a, Integral a) => a -> [Bit]
dissect i = unfoldr (\a -> if a == 0 then Nothing else Just(if a `mod` 2 == 0 then Zero else One, a `div` 2)) i

dissect':: (Num a, Ord a, Integral a) => a -> Z
dissect' i = if i < 0 then (Z Minus (dissect . abs $ i)) else (Z Plus (dissect i))

add :: Z -> Z -> Z 
add a b = dissect' . (+) (convert' a) $ convert' b

--test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]

mul :: Z -> Z -> Z
mul a b = dissect' . (*) (convert' a) $ convert' b

foo :: Bool -> Int
foo ~True = 1
foo False = 0