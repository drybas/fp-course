import Data.Char
import Control.Monad

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x | all isDigit x = Just (Number $ read x)
          | x == "+" = Just Plus
          | x == "-" = Just Minus
          | x == "(" = Just LeftBrace
          | x == ")" = Just RightBrace
          | otherwise = Nothing

--    toInt = fst . foldl (\(r, p) x -> (r + (digitToInt(x) * p), p * 10)) (0, 1) . reverse 


tokenize :: String -> Maybe [Token]
tokenize = conv . map asToken . words
    where 
        conv [] = return []
        conv (m:ms) = do 
            x <- m
            xs <- conv ms
            return (x:xs) 



type Board = [[Int]] 

nextPositions :: Board -> [Board]
nextPositions x = [[(t:xs)] | xs <- x, t <- [1, 2, 3]]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0  = []
                        | n == 0 = filter pred [b]
                        | n > 0 = do  
                            p <- nextPositions b
                            nextPositionsN p (n - 1) pred