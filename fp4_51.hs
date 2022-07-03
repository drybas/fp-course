data List a = Nil | Cons a (List a)
    deriving Show

fromList :: List a -> [a]
fromList (Cons a x) = [a] ++ fromList x  
fromList Nil = [] 

toList :: [a] -> List a
toList (x:xs) = Cons x (toList xs)
toList [] = Nil

{-- --}
data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add (Suc a) (Suc b) = Suc $ Suc $ add a b
add Zero Zero = Zero
add a Zero = a
add Zero b = b

mul :: Nat -> Nat -> Nat
mul a (Suc b) = add a (mul a b) 
mul _ Zero = Zero

fac :: Nat -> Nat
fac x@(Suc b) = mul x (fac b)
fac Zero = Suc Zero


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Node a b) = 1 + max (height a) (height b)
height (Leaf a)= 0

size :: Tree a -> Int
size (Node a b) = 1 + size a + size b
size (Leaf a) = 1

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Node a b) = (ac + bc, as + bs) 
        where 
            (ac, as) = go a
            (bc, bs) = go b
    go (Leaf a) = (1, a)