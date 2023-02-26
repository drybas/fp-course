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

tokenize' :: String -> Maybe [Token]
tokenize' s = sequence $ map asToken $ words s
--tokenize' = sequence $ asToken . words

-- Input example: 
-- tokenize "1 + ( 7 - 2 )"
