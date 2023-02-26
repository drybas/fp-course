module Demo where
import Control.Monad

newtype Writer w a = Writer { runWriter :: (a, w)}

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter 

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k = 
        let (x, u) = runWriter m
            (y, v) = runWriter $ k x
        in Writer (y, u `mappend` v)

instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM
        

