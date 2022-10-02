{-# LANGUAGE InstanceSigs #-}
module Demo where 
import Control.Monad.State

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader x) = Reader (fmap f x)

instance Applicative (Reader r) where
  pure = return 
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = ap

instance Monad (Reader r) where 
    return x = Reader $ \e -> x
    m >>= k = Reader $ \e -> 
        let v = runReader m e 
        in runReader (k v) e

ask :: Reader r r
ask = Reader id

type User = String 
type Password = String
type UserTable = [(User, Password)]

pwds :: UserTable
pwds = [("Alice", "abc"), ("Bob", "fdsfs"), ("Carol", "qruc"), ("Daniel", "123456")]

firstUser :: Reader UserTable User
firstUser = do 
    e <- ask 
    return $ fst (head e)

asks :: (r -> a) -> Reader r a 
asks = Reader

firstUserPwd :: Reader UserTable Password
firstUserPwd = do 
    pwd <- asks (snd . head)
    return pwd 

usersCount :: Reader UserTable Int
usersCount = asks length

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ runReader m . f  

localTest = do
    count1  <- usersCount 
    count2  <- local (("Mike", "12k4"):) usersCount
    return (count1, count2)

findBadPwd :: Reader UserTable [User]
findBadPwd = asks (fmap fst . filter (\x -> snd x == "123456"))

--asks (fmap (fst . filter (\x -> snd x == "123456")))
--usersWithBadPasswords :: Reader UserTable [User]
--usersWithBadPasswords = Reader   
--    count1  <- usersCount 
--    return (count1, count2)