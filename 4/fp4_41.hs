import Data.List
import Data.Char
import Data.String
import Data.Either
import Text.Read

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving(Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving(Show)

split' :: String -> String -> Either Error (String, String)
split' sub@(x:xs) s = case dropWhile (\a -> x /= a) s of 
                    "" -> Left ParsingError
                    s' -> if isPrefixOf sub s' then Right (takeWhile (\a -> x /= a) s, drop (length sub) s') else Left ParsingError
split' _ [] = Left ParsingError

buildPerson :: Either Error Person -> (String, String) -> Either Error Person 
buildPerson (Left err) _ = Left err 
buildPerson (Right p) (k,v) | k == "firstName" = Right p { firstName = v } 
                    | k == "lastName" = Right p { lastName = v }
                    | k == "age" = cvt v 
                    | otherwise = Right p
                where 
                    cvt x = case readMaybe x of 
                            Just a -> Right p { age = a }
                            Nothing -> Left (IncorrectDataError x)

-- firstName = John\nlastName = Connor\nage = 30
checkCompletness :: [(String, String)] -> Bool
checkCompletness xs = any (p "firstName") xs && any (p "lastName") xs && any (p "age") xs
                    where p = \a (b0, _) -> a == b0

parsePerson :: String -> Either Error Person
parsePerson x | not . null . lefts $ d = Left ParsingError 
              | (null . lefts $ d) && not complete = Left IncompleteDataError
              | otherwise = foldl buildPerson (Right Person {}) $ rights d
            where d = map (split' " = ") $ lines x
                  complete = checkCompletness . rights $ d 