-- Task 1
-- Let SomeType be a Monad
-- create a class Functor 

--  Some hints:
--
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- return :: Monad m => a -> m a

-- x :: SomeType a
-- f :: a -> b
-- (>>=) :: SomeType a -> (a -> SomeType c) -> SomeType c
-- x >>= f :: ???

-- (a1 -> r) -> m a1 -> m r
instance Functor SomeType where
    fmap f x = x >>= return . f

-- The Monad Laws

-- 1. return a >>= k  ≡  k a
-- 2. m >>= return    ≡  m
-- 3. m >>= k >>= k'  ≡  m >>= (|x -> k x >>= k')

